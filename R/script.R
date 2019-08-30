# library(tidyverse)
# library(fst)
# library(fs)
# library(rlang)
#' @importFrom rlang is_call call_args set_names is_function
#' @importFrom tibble tibble
#' @importFrom purrr imap map "%>%"
#' @importFrom codetools findGlobals
#' @importFrom fst read_fst write_fst
#' @importFrom fs path_ext_set path_dir dir_create
#' @importFrom pryr where
#' @export
target <- function(filepath, script) {
  # print(filepath)
  # print(formals(script))

  # Keep the script's environment on hand, but break the connection
  script_env <- environment(script)

  # Process script's args according to special arg syntax
  args <- formals(script) %>% imap(function(arg_value, arg_name) {

    ret_val <- NULL
    if (!is_call(arg_value)) {
      stop("Cannot process arg ", arg_name, " that doesn't use script arg syntax.")
    }
    call <- call_name(arg_value)
    if (call == "dep_target") {
      # (1) complain if target script is not loaded in memory
      # (2) complain if in-memory cache script doesn't match fs cache script
      # (3) load_target like normal (?)
    } else if (call == "dep_local") {
      if (length(call_args(arg_value)) == 0) {
        # If form var=local(), get var from script_env
        ret_val <- get(arg_name, envir = script_env)
      } else {
        # If form var=local(x+y), eval x+y in script_env
        ret_val <- eval(call_args(arg_value)[[1]], envir = script_env)
      }
    } else if (call == "dep_file") {
      # file_path("path/to/file.js") returns a string literal
      # file_path("path/to/:variable.js", variable = vector) returns a function
    } else {
      stop("Unrecognized call to ", call, " for arg ", arg_name)
    }

    return(ret_val)
  })

  # Process script's dependencies that aren't explicitly specified as args
  new_env_base <- parent.env(globalenv())
  browser()
  globals <- findGlobals(script) %>%
    # TODO: clunky; need to just ignore script formals to begin with
    setdiff(c("dep_target", "dep_local", "dep_file")) %>%
    set_names() %>%
    map(function(var) {
      if (!exists(var, envir = script_env)) {
        stop("Script refers to ", var, " which doesn't exist in environment")
      }
      var_val <- get(var, envir = script_env)
      var_env <- where(var, envir = script_env)
      # TODO: detect if var_env is a devtools package

      if (identical(var_env, script_env) & is_function(var_val)) {
        # If name is defined in script_env and is a function, recursively enforce purity
        return(purify_function(var_val))
      } else if (identical(var_env, script_env)) {
        # If name is defined in script_env and is not a function, bad
        stop("Script refers to external non-package variable ", var, " which is not imported")
      } else {
        # If name is defined in new_env, cache package info
        # TODO: do something with environment(var_val) which should return a namespace:pkg
        return(var_val)
      }
    })

  environment(script) <- new_environment(data = globals, parent = new_env_base)

  # Insert dummy override functions for load_target, read_*, write_*, etc.
  # that refuse b/c purity


  # Determine if script needs to be re-run. Need to check:
  # 1. The code of the script itself
  # 2. The script's args
  # 3. Any functions that the script accesses
  #   3a. Just check package versions
  #   3b. For local functions (or devtools shim functions),
  #       need to recursively track code!

  result <- do.call(script, args)
  # browser()

  ext <- case_when(
    # If file format is specified, use it
    path_ext(filepath) != "" ~ path_ext(filepath),
    # Data frames get fst by default
    is.data.frame(result) ~ "fst",
    # Use RDS as a catch-all
    TRUE ~ "rds"
  )

  filepath <- path_ext_set(filepath, ext)
  dir_create(path_dir(filepath))

  metadata <- list(ext = ext)
  if (ext == "fst") {
    write_fst(result, filepath)
    metadata$orig_class <- class(result)
    # Is there anything else that isn't preserved by fst?
  } else if (ext == "rds") {
    write_rds(result, filepath)
  } else {
    stop("Don't know how to store extension ", ext)
  }

  # TODO: store metadata in fs cache
}

purify_function <- function(func) {
  func
}

load_target <- function(name) {
  # Check hidden list in global environment, see if it has already
  # been loaded and is up to date. If it is, just return the reference
  # to it.

  # Also gc the hidden list?
}
