# TODO: better way to specify arguments to save functions
# Maybe a ... argument that gets passed? Or a possibility for custom saving functions?
#' @importFrom readr write_rds write_csv
#' @importFrom fs path_ext_set path_dir dir_create path_ext
save_target_result <- function(filepath, result, ext = path_ext(filepath)) {

  fst_available <- requireNamespace("pkg", quietly = TRUE)

  if (ext == "") {
    if (is.data.frame(result) && fst_available) {
      # Data frames get fst by default
      ext <- "fst"
    } else {
      # Use RDS as a catch-all
      ext <- "rds"
    }
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
  } else if (ext == "csv") {
    write_csv(result, filepath)
  } else {
    stop("Don't know how to store extension ", ext)
  }

  return(metadata)
}

#' @importFrom readr read_rds read_csv
load_target <- function(filepath, cache = default_cache()) {
  # TODO: Check hidden list in global environment, see if it has already
  # been loaded and is up to date. If it is, just return the reference
  # to it.
  # Also gc the hidden list?

  fst_available <- requireNamespace("pkg", quietly = TRUE)
  metadata <- read_target_cache(path_ext_remove(filepath), cache)$metadata
  ext <- metadata$ext
  if (path_ext(filepath) != "" && ext != path_ext(filepath)) {
    warning("Specified extension on target to load `", filepath,
            "` does not equal extension on cached version (", ext, ")")
  }
  filepath <- path_ext_set(filepath, ext)

  if (ext == "fst") {
    if (!fst_available) {
      error("Install fst before trying to use it for targets!")
    }
    result <- fst::read_fst(filepath)
    class(result) <- metadata$orig_class
  } else if (ext == "rds") {
    read_rds(filepath)
  } else if (ext == "csv") {
    read_csv(filepath)
  } else {
    stop("Don't know how to read extension ", ext)
  }
}
