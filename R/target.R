#' @importFrom tibble tibble
#' @importFrom digest digest
#' @export
target <- function(filepath, method, cache = get_cache()) {

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
  target_hash <- read_cache(cache)$targets[[filepath]]$hash

  if (length(target_hash) && target_hash == digest(pure_method$trackables)) {
    cat("Target `", filepath, "` does not need updating. ", sample(encouragement, 1), "\n", sep = "")
    # TODO: return?
  } else {
    # If needs freshening up, rerun the method and save the results!
    start_time <- Sys.time()
    result <- do.call(pure_method$value, args)
    end_time <- Sys.time()

    save_target_result(filepath, result)
    update_cache(
      filepath,
      cache = cache,
      cache_val = list(
        hash = digest(pure_method$trackables),
        metadata = list(
          elapsed = end_time - start_time
        )
      )
    )
    return(result)
  }

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

encouragement <- c(
  "Huzzah!",
  "Great news :)",
  "Good job.",
  "Nice work.",
  "Chill B^)"
)
