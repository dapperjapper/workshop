#' @importFrom digest digest
#' @importFrom dplyr combine
#' @importFrom tidyr expand_grid
#' @importFrom purrr transpose walk
#' @importFrom rlang env_bind
#' @importFrom fs path_ext_remove
#' @export
target <- function(filepath_spec, method, cache = get_cache()) {

  dimensions <- spec_dimensions(filepath_spec)
  ext <- path_ext(filepath_spec)

  # Process method's args according to special arg syntax
  args <- process_method_args(method, cache)

  # Process method's dependencies that aren't explicitly specified as args,
  # and bake them into the function environment
  pure_method <- purify_function(method)

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
        warning("For dimension `", dim, "`, the specified values are not equal between args...")
      }
      dim_values_for_args %>% reduce(intersect)
    })

  unspecified_dimensions <- setdiff(dimensions, names(specified_dimensions))

  # Dummy dimension if none present
  if (length(specified_dimensions) == 0) {
    specified_dimensions = list(id = T)
  }

  # Loop over every combination of all the specified dimensions
  expand_grid(!!!specified_dimensions) %>% transpose() %>% walk(function(these_dims) {

    # Fill in the dimensions we have, leave others still in :dimension format
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

    # Maybe this is necessary?? If only extension in filepath is changed, nothing will
    # invalidate cache...
    pure_method$trackables$ext <- ext

    # Get the hash of this run
    trackables_hash <- digest(pure_method$trackables)

    # Get the hashes of the last run
    # (there may be multiple bc of unspecified dimensions... so
    # we must check that hash is equal across all these)
    target_hash <- read_matching_targets_cache(path_ext_remove(filepath_spec_partial), cache) %>%
      map("hash") %>%
      unique()

    if (length(target_hash) != 1) {
      target_hash <- ""
    }

    # Return if target is up to date
    if (length(target_hash) && target_hash == trackables_hash) {
      cat("Target `", path_ext_remove(filepath_spec_partial), "` is up to date. ",
          sample(encouragement, 1), "\n", sep = "")
      return()
    }

    # OK let's build this frickin target then

    loaded_args <- map(args, "load") %>%
      map(do.call, args = these_dims)

    save_target <- function(result, ...) {
      dim_str <- list(...) %>%
        imap(function(x, i) { str_c(i, '="', x, '"') }) %>%
        str_c(collapse = ", ")
      cat("Saving", dim_str, "...\n")
      end_time <- Sys.time()
      filepath <- encode_spec(list(...), filepath_spec_partial)
      metadata <- save_target_result(filepath, result)
      upsert_target_cache(
        cache = cache,
        target = path_ext_remove(filepath),
        val = list(
          hash = trackables_hash,
          elapsed = end_time - start_time,
          metadata = metadata
        )
      )
      # Double assignment sets start_time at the top level
      start_time <<- Sys.time()
    }

    # Special values for use inside method
    env_bind(
      .env = environment(pure_method$value),
      .dimensions = these_dims,
      save_target = save_target
    )

    # Git 'r dun
    cat("Running target `", path_ext_remove(filepath_spec_partial), "`\n", sep = "")
    start_time <- Sys.time()
    ret_val <- do.call(pure_method$value, loaded_args)

    cat("Complete!\n")
  })

}

#' @importFrom purrr imap map "%>%"
#' @importFrom rlang is_call call_args set_names is_function call_name expr sym
process_method_args <- function(method, cache) {
  args <- formals(method)
  method_env <- environment(method)

  args %>% imap(function(arg_value, arg_name) {

    # Each arg comes with a loader to load the value for a given dimension,
    loader <- function(...) { NULL }
    # a hasher to determine if a arg value has changed for a given dimension (without loading),
    hasher <- function(...) { NULL }
    # and the dimensions that a given arg can take on! :^)
    dimensions <- list()
    if (!is_call(arg_value)) {
      stop("Cannot process arg ", arg_name, " that doesn't use method arg syntax.")
    }

    call <- call_name(arg_value)
    if (call == "dep_target") {

      # Eval anything put in the declaration in method_env
      target_spec <- eval(call_args(arg_value)[[1]], envir = method_env)
      # TODO: at what level are dimensions specified? Allow flexibility between runs
      # We know the hash across unspecified dimensions at this scope, but we need to
      # wait until hasher() is run across all the specified dimensions to see what
      # the hash looks like with those dimensions embedded
      hasher <- function(...) {
        target_path <- encode_spec(list(...), target_spec)
        read_target_cache(path_ext_remove(target_path), cache)$hash
      }

      # TODO TODO TODO
      # (1) complain if target method is not loaded in memory
      # (2) complain if in-memory cache method doesn't match fs cache method
      # (3) load_target like normal (?)
      loader <- function(...) {
        target_path <- encode_spec(list(...), target_spec)
        load_target(target_path, cache)
      }

    } else if (call == "dep_local") {

      # Two syntaxes for dep_local():
      if (length(call_args(arg_value)) == 0) {
        # If form var=dep_local(), get var from method_env
        val <- get(arg_name, envir = method_env)
      } else {
        # If form var=dep_local(x+y), eval x+y in method_env
        val <- eval(call_args(arg_value)[[1]], envir = method_env)
      }
      # TODO: We could defer loading local from method_env until loader() is run
      # not sure this would make any difference...
      loader <- function(...) { val }
      hasher <- function(...) { digest(val) }

    } else if (call == "dep_file") {

      # STOP if the dep is found in the cache of targets
      # file_path("path/to/file.js") returns a function()
      # file_path("path/to/:variable.js", variable = vector) returns a function(variable)

    } else if (call == "dimension") {

      # This turns dimension(vector, c("reg", 23), 1:100) into c(vector, c("reg", 23), 1:100)
      # Then the c() call is evaluated in the context of method_env. This is a super flexible
      # syntax! Need to make sure we end up w a character vector, though.
      arg_value[[1]] <- expr(c)
      dimensions[[arg_name]] <- eval(arg_value, envir = method_env)
      # The loader is just a function that returns the value of its dimension as specified in the
      # calling "..."
      loader <- eval(expr(function(...) {
        list(...)[[!!arg_name]]
      }))
      hasher <- function(...) {
        digest(loader(...))
      }

    } else {
      stop("Unrecognized call to ", call, " for arg ", arg_name)
    }

    return(list(load = loader, hash = hasher, dimensions = dimensions))
  })
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
  "Chill B^)"
)
