#' PURITY
#'
#' What is allowed inside of target methods:
#' - Explicitly specified dependencies
#'   - Objects                                Invalidated through inequality
#'   - File paths                             Invalidated through file modified dates
#'   - Other targets                          Invalidated recursively
#'   - Dirty functions from devtools shim /   Must specify cache invalidation method
#'     method source environment
#' - Functions from packages                  Invalidated through package versioning
#' - Pure functions from devtools shim /      Invalidated through recursive code analysis
#'   method source environment
#'
#' What is not allowed inside of target methods:
#' - Unspecified non-function objects from method source environment
#' - Loading from unspecified file paths
#' - Other targets
#'
#' @importFrom rlang new_environment env_name is_primitive env_parents empty_env
#' @importFrom purrr map_chr discard
#' @importFrom codetools findGlobals
#' @importFrom pryr where
#' @importFrom stringr str_remove str_detect
purify_function <- function(func, ignore_arg_defaults = T) {

  # TODO: function "blacklist" that will throw an error if discovered
  # recursively within the function. Comes coupled with namespace, so
  # readr::write_csv will error out but globally namespaced write_csv won't.
  # Function blacklist is necessary to prevent user from breaking purity.

  func_env <- environment(func)
  new_env_base <- empty_env() # parent.env(globalenv())

  # CODE ANALYSIS (THIS IS THE HARD PART)
  globals <- findGlobals(func)

  # If we don't do this, complains that `dep_target` (for example)
  # doesn't exist in the environment. This is because we are blurring
  # the distinction between `method` (a specialized function written
  # to make a target) and a function more generally.
  if (ignore_arg_defaults) {
    # TODO: clunky; need to just ignore func formals to begin with
    globals <- setdiff(globals, c("dep_target", "dep_local", "dep_file", "dimension", "save_target", ".dimensions"))
  }

  globals <- globals %>%
    set_names() %>%
    map(function(var) {
      if (!exists(var, envir = func_env)) {
        warning("Function refers to ", var, " which doesn't exist in environment")
        return(list(value = NULL, trackables = "(missing)"))
      }
      var_val <- get(var, envir = func_env)
      # var_env <- where(var, env = func_env)
      # TODO: better methodology for figuring out if func comes from package
      # TODO: detect if var_env is a devtools package

      if (!is_function(var_val)) {
        # If name is not a function, bad
        stop("Function refers to external non-function variable `", var, "` which is not imported")
      } else if (exists(var, envir = func_env, inherits = F)) {
        # If name is defined in func_env and is a function, recursively enforce purity
        return(purify_function(var_val, ignore_arg_defaults = F))
      } else if (is_primitive(var_val)) {
        # If it's just a primitive, don't worry about it
        return(list(value = var_val, trackables = "(primitive)"))
      } else {
        # If name is defined outside func_env, cache package info

        # TODO: maybe we can track down the package for non-functions as well??
        # we can't use environment(var_val) but maybe we can look at the defining env??
        # defining_env <- NULL
        # for (env in env_parents(func_env, last = NULL)) {
        #   print(exists(var, envir = env, inherits = F))
        #   if (exists(var, envir = env, inherits = F)) {
        #     print(env)
        #     defining_env <- env
        #     break
        #   }
        # }
        package_name <- environment(var_val) %>%
          # Some functions are imported from other packages through some weird
          # mechanisms (see `%>%` in purrr) so we must traverse up thru
          # environments to see where it comes from.
          c(
            .,
            env_parents(.)
          ) %>%
          map_chr(env_name) %>%
          # Take the highest level environment with a namespace signifier
          .[which(str_detect(., "^namespace:"))] %>%
          head(1) %>%
          str_remove("^namespace:")

        if (!length(package_name) || package_name == "") {
          stop("Could not find source package for function `", var, "` to track dependency...")
        }

        # TODO: track things better than just version?
        return(list(value = var_val, trackables = list(
          package = package_name,
          version = packageVersion(package_name)
        )))
      }
    })

  # TODO: globals_to_embed = everything from globals that isn't in an environment that's
  # a parent of new_env_base. Or, just move everything and have new_env_base be the empty
  # env? Need to test speed...

  # Load globals into function environment so it can access those and *only* those
  environment(func) <- new_environment(data = map(globals, "value"), parent = new_env_base)

  # Purified function and trackables
  return(list(
    value = func,
    trackables = list(
      body = body(func),
      globals = map(globals, "trackables") %>%
        # The character trackables are either "(missing)"
        # or "(primitive)" which are useful for debugging
        # but not actually needed for tracking.
        discard(is.character)
    )
  ))
}

