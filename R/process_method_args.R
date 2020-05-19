#' @importFrom purrr imap map "%>%"
#' @importFrom rlang is_call call_args set_names is_function call_name expr sym
process_method_args <- function(method, cache) {
  args <- formals(method)
  method_env <- environment(method)

  # TODO: bake `cache` at beginning of loop so that yaml only has to be read once...

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
      target_spec <- eval(call_args(arg_value)[[1]], envir = method_env)[[1]]
      cached_targets <- read_matching_targets_cache(target_spec, cache = cache)

      # TODO: what if there aren't any cached targets that match the spec?

      # The dimensions that the dep_target operates over
      # are extracted from the cached targets
      dimensions <- cached_targets %>%
        names() %>%
        map(decode_spec, spec = target_spec) %>%
        transpose() %>%
        map(~ unique(combine(.)))

      hasher <- function(...) {
        target_path <- encode_spec(list(...), target_spec)
        cached_targets[[path_ext_remove(target_path)]]$hash
      }

      # TODO TODO TODO
      # (1) complain if target method is not loaded in memory
      # (2) complain if in-memory cache method doesn't match fs cache method
      # (3) load_target like normal (?)
      loader <- function(...) {
        target_path <- encode_spec(list(...), target_spec)
        cat("Loading `", target_path, "` ...\n", sep = "")
        load_target(target_path, cache)
      }

    } else if (call == "dep_rollup") {

      # Eval anything put in the declaration in method_env
      target_spec <- call_args(arg_value) %>%
        # Drop the named arguments
        .[names(.)==""] %>%
        # Only the first unnamed argument
        .[[1]] %>%
        eval(envir = method_env) %>%
        # Only the first item of the evaluated bit
        .[[1]]
      cached_targets <- read_matching_targets_cache(target_spec, cache = cache)

      # TODO: informative error when "across" is forgotten
      across_dimensions <- call_args(arg_value) %>%
        .[names(.)=="across"] %>%
        .[[1]] %>%
        eval(envir = method_env)

      # TODO: format argument?
      # They could specify `dep_rollup(format = "tibble")`
      # for dimensions as columns & data list-col, or
      # `dep_rollup(format = "list")` for
      # `data$dimension1$dimension2` format.
      # Maybe also have `format = "loader"`, for a lazy-loader
      # type deal?

      # The dimensions that the dep_rollup operates over
      # are extracted from the cached targets
      all_dimensions <- cached_targets %>%
        names() %>%
        map(decode_spec, spec = target_spec) %>%
        transpose() %>%
        map(~ unique(combine(.)))

      # Drop dimensions that we are rolling up over,
      # since we won't need to iterate over them as targets
      dimensions <- all_dimensions %>%
        .[!names(.) %in% across_dimensions]

      hasher <- function(...) {
        target_path <- encode_spec(list(...), target_spec, allow_missing = T)
        # TODO clean this code
        # There can be multiple responding targets for any combination of ... arguments
        # So must pull all of their hashes and combine
        matches <- cached_targets[spec_match(names(cached_targets), target_path)]
        matches %>%
          map_chr("hash") %>%
          .[order(names(.))] %>%
          digest()
      }

      load_list_recursive <- function(
        target_partial,
        across_dimensions,
        these_dimensions = list()
      ) {

        # If we are at the bottom of the tree, actually load the target
        if (length(across_dimensions) == 0) {
          target_path <- encode_spec(these_dimensions, target_partial)
          cat("Loading `", target_path, "` ...\n", sep = "")
          # TODO: what if this combination of dimensions doesn't actually exist?
          return(load_target(target_path, cache))
        }

        # ELSE, take the first dimension off the stack
        this_dim_name <- across_dimensions[[1]]
        this_dim <- all_dimensions[[this_dim_name]]

        # Some dimensions aren't referenced by the given target, so
        # they won't be used by `encode_spec`. We can just drop them
        # from the tree and move on.
        if (this_dim %>% is.null()) {
          return(load_list_recursive(
            target_partial,
            across_dimensions %>% tail(-1),
            these_dimensions
          ))
        }

        # Otherwise, branch the tree and recurse
        this_dim %>%
          set_names() %>%
          map(~ load_list_recursive(
            target_partial,
            across_dimensions %>% tail(-1),
            {these_dimensions[this_dim_name] <- .; these_dimensions}
          ))
      }

      # TODO TODO TODO
      # (1) complain if target method is not loaded in memory
      # (2) complain if in-memory cache method doesn't match fs cache method
      # (3) load_target like normal (?)
      loader <- function(...) {
        target_partial <- encode_spec(list(...), target_spec, allow_missing = T)
        load_list_recursive(target_partial, across_dimensions)
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
      # The loader is just a function that returns the value of its dimension
      # as specified in the calling "..."
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
