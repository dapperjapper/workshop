#' @importFrom tibble tibble
#' @export
target <- function(filepath, method) {

  # Process method's args according to special arg syntax
  args <- formals(method) %>% process_method_args(environment(method))

  # Process method's dependencies that aren't explicitly specified as args
  pure_method <- purify_function(method)

  # Determine if method needs to be re-run. Need to check:
  # 1. The code of the method itself
  # 2. The method's args
  # 3. Any functions that the method accesses
  #   3a. Just check package versions
  #   3b. For local functions (or devtools shim functions),
  #       need to recursively track code!

  result <- do.call(pure_method$value, args)

  save_target_result(filepath, result)
  return(result)
}

#' @importFrom purrr imap map "%>%"
#' @importFrom rlang is_call call_args set_names is_function call_name
process_method_args <- function(args, method_env) {
  args %>% imap(function(arg_value, arg_name) {

    ret_val <- NULL
    if (!is_call(arg_value)) {
      stop("Cannot process arg ", arg_name, " that doesn't use method arg syntax.")
    }
    call <- call_name(arg_value)
    if (call == "dep_target") {
      # (1) complain if target method is not loaded in memory
      # (2) complain if in-memory cache method doesn't match fs cache method
      # (3) load_target like normal (?)
    } else if (call == "dep_local") {
      if (length(call_args(arg_value)) == 0) {
        # If form var=local(), get var from method_env
        ret_val <- get(arg_name, envir = method_env)
      } else {
        # If form var=local(x+y), eval x+y in method_env
        ret_val <- eval(call_args(arg_value)[[1]], envir = method_env)
      }
    } else if (call == "dep_file") {
      # file_path("path/to/file.js") returns a string literal
      # file_path("path/to/:variable.js", variable = vector) returns a function
    } else {
      stop("Unrecognized call to ", call, " for arg ", arg_name)
    }

    return(ret_val)
  })
}

#' @importFrom rlang new_environment env_name is_primitive env_parents empty_env
#' @importFrom purrr map_chr
#' @importFrom codetools findGlobals
#' @importFrom pryr where
#' @importFrom stringr str_remove str_detect
purify_function <- function(func, ignore_arg_defaults = T) {

  # TODO: function "blacklist" that will throw an error if discovered
  # recursively within the function. Comes coupled with namespace, so
  # readr::write_csv will error out but globally namespaced write_csv
  # won't

  # Function blacklist is necessary to prevent user from breaking purity.

  func_env <- environment(func)
  new_env_base <- empty_env() # parent.env(globalenv())

  # CODE ANALYSIS (THIS IS THE HARD PART)
  globals <- findGlobals(func)

  # If we don't do this, complains that
  if (ignore_arg_defaults) {
    # TODO: clunky; need to just ignore func formals to begin with
    globals <- setdiff(globals, c("dep_target", "dep_local", "dep_file"))
  }

  globals <- globals %>%
    set_names() %>%
    map(function(var) {
      if (!exists(var, envir = func_env)) {
        warning("Function refers to ", var, " which doesn't exist in environment")
        return(list(value = NULL, trackables = "(unknown)"))
      }
      var_val <- get(var, envir = func_env)
      # var_env <- where(var, env = func_env)
      # TODO: better methodology for figuring out if func comes from package
      # TODO: detect if var_env is a devtools package

      if (exists(var, envir = func_env, inherits = F) & is_function(var_val)) {
        # If name is defined in func_env and is a function, recursively enforce purity
        return(purify_function(var_val, ignore_arg_defaults = F))
      } else if (exists(var, envir = func_env, inherits = F)) {
        # If name is defined in func_env and is not a function, bad
        stop("Function refers to external non-package variable ", var, " which is not imported")
      } else if (is_primitive(var_val)) {
        # If it's just a primitive, don't worry about it
        return(list(value = var_val, trackables = "(primitive)"))
      } else {
        # If name is defined outside func_env, cache package info

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

  # browser()
  # TODO: globals_to_embed = everything from globals that isn't in an environment that's
  # a parent of new_env_base

  # Load globals into function environment so it can access those and *only* those
  environment(func) <- new_environment(data = map(globals, "value"), parent = new_env_base)
  # Purified function and trackables
  return(list(
    value = func,
    trackables = list(
      body = body(func),
      formals = formals(func),
      globals = map(globals, "trackables")
    )
  ))
}

#' @importFrom readr write_rds
#' @importFrom fs path_ext_set path_dir dir_create path_ext
save_target_result <- function(filepath, result) {

  fst_available <- requireNamespace("pkg", quietly = TRUE)

  if (path_ext(filepath) != "") {
    # If file format is specified, use it
    ext <- path_ext(filepath)
  } else if (is.data.frame(result) && fst_available) {
    # Data frames get fst by default
    ext <- "fst"
  } else {
    # Use RDS as a catch-all
    ext <- "rds"
  }

  filepath <- path_ext_set(filepath, ext)
  dir_create(path_dir(filepath))

  metadata <- list(ext = ext)
  if (ext == "fst") {
    if (!fst_available) {
      error("Install fst before trying to use it for targets!")
    }
    fst::write_fst(result, filepath)
    metadata$orig_class <- class(result)
    # Is there anything else that isn't preserved by fst?
  } else if (ext == "rds") {
    write_rds(result, filepath)
  } else {
    stop("Don't know how to store extension ", ext)
  }

  # TODO: store metadata in fs cache
}

load_target <- function(name) {
  # Check hidden list in global environment, see if it has already
  # been loaded and is up to date. If it is, just return the reference
  # to it.

  # Also gc the hidden list?
}
