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
