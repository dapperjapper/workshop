#' @importFrom fs file_create file_exists
#' @export
get_cache <- function(path = ".workshop") {
  # TODO config session cache? so don't have to specify with every target
  dir_create(path_dir(path))
  if (!file_exists(path)) {
    write_rds(list(cache_version = "0.01", targets = list()), path)
  }
  return(structure(list(path = path), class="workshop_cache"))
}

#' @export
print.workshop_cache <- function(cache) {
  cat("Path to cache:", cache$path, "\n")
}

#' @importFrom readr read_rds
read_cache <- function(cache) {
  readRDS(cache$path)
}

update_cache <- function(filepath, cache, cache_val) {
  data <- read_rds(cache$path)
  data$targets[[filepath]] <- cache_val
  write_rds(data, cache$path)
}
