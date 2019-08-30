# library(tidyverse)
# library(fst)
# library(fs)
# library(rlang)
#' @import fs fst rlang
target <- function(filepath, script) {
  # print(filepath)
  # print(formals(script))

  # Keep the script's environment on hand, but break the connection
  script_env <- environment(script)
  environment(script) <- parent.env(globalenv())

  # Process script's args according to special arg syntax
  args <- imap(formals(script), function(arg_value, arg_name) {

    if (!is_call(arg_value)) {
      stop("Cannot process arg ", arg_name, " that doesn't use script arg syntax.")
    }
    call <- call_name(arg_value)
    if (call == "load_target") {

    } else if (call == "local") {

    } else if (call == "file_path") {
      # TODO: implement
      # file_path("path/to/file.js") returns a string literal
      # file_path("path/to/:variable.js", variable = vector) returns a function
    } else {
      stop("Unrecognized call to ", call, " for arg ", arg_name)
    }

    print(arg_name)
    print(arg_value)
    as.character(arg_value)
    browser()
  })

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

  if (ext == "fst") {
    write_fst(result, filepath)
  } else if (ext == "rds") {
    write_rds(result, filepath)
  } else {
    stop("Don't know how to store extension ", ext)
  }
}

load_target <- function(name) {
  # Check hidden list in global environment, see if it has already
  # been loaded and is up to date. If it is, just return the reference
  # to it.

  # Also gc the hidden list?
}
