workshop_sesh <- new.env(parent = emptyenv())
# Will default to using the default cache when first referenced
env_bind_lazy(workshop_sesh, cache = get_cache())

set_default_cache <- function(cache) {
  workshop_sesh$cache <- cache
}

default_cache <- function() {
  workshop_sesh$cache
}

#' @importFrom rlang env_bind_lazy
#' @importFrom fs file_create file_exists
#' @importFrom yaml read_yaml write_yaml
#' @export
get_cache <- function(path = "workshop_cache.yaml") {
  cat("Loading cache", path, "\n")
  dir_create(path_dir(path))
  if (!file_exists(path)) {
    write_yaml(list(cache_version = "0.01", targets = list()), path)
  }
  return(structure(list(path = path), class="workshop_cache"))
}

#' @export
print.workshop_cache <- function(cache) {
  cat("<cache with path:", cache$path, ">\n")
}

read_target_cache <- function(target, cache = default_cache()) {
  read_yaml(cache$path)$targets[[target]]
}

read_matching_targets_cache <- function(spec_partial, cache = default_cache()) {
  # TODO
}

upsert_target_cache <- function(target, val, cache = default_cache()) {
  data <- read_yaml(cache$path)
  data$targets[[target]] <- val
  write_yaml(data, cache$path)
}
