#' @importFrom digest digest
#' @importFrom dplyr combine
#' @importFrom tidyr expand_grid
#' @importFrom purrr transpose walk reduce2
#' @importFrom rlang env_bind rep_along
#' @importFrom fs path_ext_remove
#' @importFrom stringr str_trim
#' @importFrom future future value resolved
#' @export
target <- function(filepath_spec, method,
                   cache = default_cache(),
                   log_trackables = F, force_rebuild = F) {

  dimensions <- spec_dimensions(filepath_spec)
  ext <- path_ext(filepath_spec)

  # Process method's args according to special arg syntax
  args <- process_method_args(method, cache)

  # Process method's dependencies that aren't explicitly specified as args,
  # and bake them into the function environment
  pure_method <- purify_function(method)
  # Maybe this is necessary?? If only extension in filepath is changed, nothing will
  # invalidate cache...
  pure_method$trackables$ext <- ext

  # What are the various dimensions that each arg operates over?
  # Dimension args operate over themselves, other args that reference
  # dimensions may operate over multiple dimensions
  arg_dimensions <- map(args, "dimensions")

  # For each dimension, retrieve all the values that it can take.
  # For example, consider the below arg_dimensions:
  #   arg_dimensions <-
  #     list(
  #       size = list(size = c("big", "small")),
  #       name = list(name = c("ed",
  #                            "edd", "eddy")),
  #       some_file = list(name = "ed", size = "big"),
  #       local_var = NULL
  #     )
  # This would give specified dimenions list(size = "big", name = "ed").
  # Since the name dimension isn't repeated in full for the some_file arg,
  # we limit it to just the values that ARE reported.
  specified_dimensions <- arg_dimensions %>%
    map(names) %>%
    combine() %>%
    unique() %>%
    set_names() %>%
    map(function(dim) {
      dim_values_for_args <- map(arg_dimensions, dim) %>% discard(is.null)

      if (length(dim_values_for_args) > 1 && !dim_values_for_args %>% reduce(identical)) {
        warning(
          "For dimension `", dim, "`, the specified values are not equal between args...")
      }
      dim_values_for_args %>% reduce(intersect)
    })

  dimensions_missing_in_spec <- setdiff(names(specified_dimensions), dimensions)
  if (length(dimensions_missing_in_spec)) {
    stop(
      "Dimensions ",
      str_c("`", dimensions_missing_in_spec, "`", collapse = ", "),
      " need to be present in the target spec,",
      " or you must explicitly aggregate over them using dep_rollup."
    )
  }
  # The converse of the above
  # dynamic_branching_dimensions <- setdiff(dimensions, names(specified_dimensions))

  # Dummy dimension if none present
  if (length(specified_dimensions) == 0) {
    specified_dimensions = list(id = T)
  }

  # Loop over every combination of all the specified dimensions
  # TODO: is this appropriate behavior?
  futures <- expand_grid(!!!specified_dimensions) %>%
    transpose() %>%
    reduce2(., 1:length(.), .init = list(), function(past_futures, these_dims, i) {
      # Loop over the colors and create printer
      color <- job_colors[[(i-1)%%length(job_colors)+1]]
      dim_str <- list_to_str(these_dims)

      if (identical(these_dims, list(id = T))) {
        these_dims <- list()
        dim_str <- "\u2600" # idk
      }

      printer <- function(...) {
        cat(color("[", dim_str, "] ", ..., "\n", sep = ""))
      }

      this_future <- run_target(
        these_dims,
        printer = printer,
        filepath_spec = filepath_spec,
        pure_method = pure_method,
        args = args,
        options = list(
          log_trackables = log_trackables,
          force_rebuild = force_rebuild
        ),
        cache = cache
      )

      # What a confusing variable name! Who came up with that?
      # Basically we throw our new asyncronous future on the pile,
      # and then we go back and check to see if anything on the
      # pile has resolved.
      past_futures <- c(past_futures, this_future)
      map(past_futures, function(pending_future) {
        # If it's resolved, get the value.
        # This prints anything that was logged. It's useful to do this
        # as soon as we see it was resolved so logging is as
        # contemporaneous as possible
        if (resolved(pending_future)) {
          value(pending_future)
        }
      })

      return(past_futures)
    })
    # clustermq::Q(
    #   run_target,
    #   these_dims = .,
    #   # This will repeat over the list to cover all jobs
    #   color = rep_along(., job_colors),
    #   n_jobs = n_jobs,
    #   const = list(
    #     filepath_spec = filepath_spec,
    #     pure_method = pure_method,
    #     args = args,
    #     log_trackables = log_trackables,
    #     cache = cache
    #   )
    # )

  # TODO: remove targets from yaml that fit spec but were not touched?
  # and remove their files?

  # Block until everything is complete
  futures %>% map(value)
  invisible(futures)
}

run_target <- function(these_dims, printer,
                       filepath_spec, pure_method, args, options, cache) {

  # TODO: try/catch
  # Fill in the dimensions we have in the path, leave others still in :dimension format
  filepath_spec_partial <- encode_spec(these_dims, filepath_spec, allow_missing = T)

  # Determine if method needs to be re-run. Need to check:
  # 1. The code of the method itself
  # 2. The method's args
  # 3. Any functions that the method accesses
  #   3a. Just check package versions
  #   3b. For local functions (or devtools shim functions),
  #       need to recursively track code!

  # Assemble the hashes for the method's args (dependent on dimension)
  pure_method$trackables$formals <- map(args, "hash") %>%
    map(do.call, args = these_dims)

  if (options$log_trackables) {
    trackables_dir <- str_c(path_dir(filepath_spec_partial), "/trackables")
    dir_create(trackables_dir)
    write_rds(pure_method$trackables,
              str_c(trackables_dir, "/", path_file(filepath_spec_partial), "_trackables.rds"))
  }

  # Get the hash of this run
  trackables_hash <- digest(pure_method$trackables)

  # Get the hashes of the last run
  # (there may be multiple bc of unspecified dimensions... so
  # we must check that hash is equal across all these)
  target_hash <- path_ext_remove(filepath_spec_partial) %>%
    read_matching_targets_cache(cache) %>%
    map("hash") %>%
    unique()

  if (length(target_hash) != 1) {
    target_hash <- ""
  }

  # Return if target is up to date
  if (length(target_hash) &&
      target_hash == trackables_hash &&
      !options$force_rebuild) {

    # TODO: Check that the result files still exist??
    printer("Target is up to date. ", # `", path_ext_remove(filepath_spec_partial), "`
        sample(encouragement, 1))
    return()
  }

  # OK let's build this frickin target then
  printer("Running target...") #`", path_ext_remove(filepath_spec_partial), "`
  start_time <- Sys.time()
  times <- list()

  loaded_args <- map(args, "load") %>%
    map(do.call, args = these_dims)

  timer_phase_end <- function(phase_name = "Unnamed phase") {
    end_time <- Sys.time()

    # Come up with a unique name for the phase
    name_suffix <- ""
    while (str_trim(str_c(phase_name, " ", name_suffix)) %in% names(times)) {
      if (name_suffix == "") {
        name_suffix <- 2
      } else {
        name_suffix <- name_suffix + 1
      }
    }
    mins <- as.numeric(end_time - start_time, units = "mins")
    times[[str_trim(str_c(phase_name, " ", name_suffix))]] <<- mins

    # Double assignment sets start_time at the top level
    start_time <<- Sys.time()
    return(mins)
  }

  # TODO: only do this if there *were* dependencies to load
  timer_phase_end("Loading dependencies")

  # TODO what to do when func doesn't have a save_target in it?
  save_target <- function(result, ...) {
    timer_phase_end("Processing")
    printer("Saving ", list_to_str(list(...)), "...")

    # TODO: error when unnecessary dimensions provided?
    filepath <- encode_spec(list(...), filepath_spec_partial)
    metadata <- save_target_result(filepath, result)

    timer_phase_end("Saving")
    upsert_target_cache(
      cache = cache,
      target = path_ext_remove(filepath),
      val = list(
        hash = trackables_hash,
        build_min = times,
        metadata = metadata
      )
    )
    times <<- list()
    start_time <<- Sys.time()
  }

  # Special values for use inside method
  environment(pure_method$value) %>%
    env_bind(
      .dimensions = these_dims,
      .cache = cache,
      .realcat = cat,
      cat = printer,
      save_target = save_target,
      timer_phase_end = timer_phase_end
    )

  packages_to_load <- pure_method$trackables$globals %>%
    map_chr("package") %>%
    unique()

  # Git 'r dun
  this_future <- future(
    {
      do.call(pure_method$value, loaded_args)
      printer("Complete!")
    }
  )

  return(this_future)
}

# Dummy function
#' @export
save_target <- function(...) {
  stop("Please only use save_target() inside a target!")
}

encouragement <- c(
  "Huzzah!",
  "Great news :)",
  "Good job.",
  "Nice work.",
  "Chill B^)",
  "Stan SOPHIE and 100 gecs",
  "You're doing a great job ;^)",
  "Remember to drink water!"
)

job_colors <- list(
  # crayon::red, # Looks too much like an error
  crayon::green,
  crayon::yellow,
  crayon::blue,
  crayon::magenta,
  crayon::cyan,
  crayon::green$underline,
  crayon::yellow$underline,
  crayon::blue$underline,
  crayon::magenta$underline,
  crayon::cyan$underline
)

list_to_str <- function(l) {
  l %>%
    imap(function(x, i) { str_c(i, '="', x, '"') }) %>%
    str_c(collapse = ", ")
}