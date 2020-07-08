workshop_sesh <- new.env(parent = emptyenv())
# Will default to using the default cache when first referenced
env_bind_lazy(workshop_sesh, cache = get_cache("workshop_cache.yaml"))

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
get_cache <- function(path) {
  cat("Getting cache:", path, "\n")
  dir_create(path_dir(path))
  if (!file_exists(path)) {
    write_yaml(list(cache_version = "0.01", targets = list()), path)
  }
  return(structure(list(path = path, loaded = F), class="workshop_cache"))
}

#' @export
as.character.workshop_cache <- function(x, ...) {
  return(str_c("<workshop cache with path: ", x$path, ">\n", ...))
}

# TODO: error if cache permissions issue?
load_cache <- function(cache) {
  cat("Loading cache:", cache$path, "\n")
  cache$data <- read_yaml(cache$path)
  cache$loaded <- T
  return(cache)
}

write_cache <- function(cache) {
  if (!cache$loaded) {
    stop("Cache must be loaded before writing: ", cache)
  }
  write_yaml(cache$data, cache$path)
}

read_target_cache <- function(target, cache = default_cache()) {
  if (!cache$loaded) {
    stop("Cache must be loaded before reading: ", cache)
  }
  cache$data$targets[[target]]
}

# TODO: remove spec_partial extension before reading?
read_matching_targets_cache <- function(spec_partial, cache = default_cache()) {
  if (!cache$loaded) {
    stop("Cache must be loaded before reading: ", cache)
  }
  targets <- cache$data$targets
  targets[spec_match(names(targets), spec_partial)]
}

modify_cache <- function(target, val, cache = default_cache()) {
  if (!cache$loaded) {
    stop("Cache must be loaded before modifying: ", cache)
  }
  cache$data$targets[[target]] <- val
  return(cache)
}
