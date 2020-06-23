workshop_sesh <- new.env(parent = emptyenv())
# Will default to using the default cache when first referenced
env_bind_lazy(workshop_sesh, cache = get_cache("workshop_cache.yaml"))

#' @export
set_default_cache <- function(cache) {
  workshop_sesh$cache <- cache
}

#' @export
default_cache <- function() {
  workshop_sesh$cache
}

#' @importFrom rlang env_bind_lazy
#' @importFrom fs file_create file_exists
#' @importFrom yaml write_yaml
#' @export
get_cache <- function(path) {
  cat("Loading cache:", path, "\n")
  if (!file_exists(path)) {
    dir_create(path_dir(path))
    # Skeleton for new caches
    write_yaml(list(cache_version = "0.01", targets = list()), path)
  }
  return(structure(list(path = path), class="workshop_cache"))
}

#' @export
print.workshop_cache <- function(x, ...) {
  cat("<workshop cache with path:", x$path, ">\n", ...)
}

#' Allocate/deallocate Cache
#'
#' This system allows reading from the cache on the filesystem
#' while storing the data in memory when possible. This reduces
#' filesystem traffic while minimizing risk that cache will be
#' changed while code is executing.
#'
#' You shouldn't have to use these functions unless you are doing
#' a lot of cache work. They are called behind the scenes in the
#' `target` function, and any other function that accesses the cache.
#'
#' @importFrom rlang "%||%"
#' @importFrom yaml read_yaml
alloc_cache <- function(cache, force = F) {
  if (class(cache) != "workshop_cache") stop("Must pass in a `workshop_cache`!")
  # If alloc level is missing or is zero
  if ((cache$alloc_level %||% 0) <= 0 || force) {
    # Make sure the file exists
    # browser()
    # get_cache(cache$path)
    cache$alloc_level <<- 1L
    cache$data <<- read_yaml(cache$path)
  } else {
    cache$alloc_level <<- cache$alloc_level + 1L
  }
}

#' @describeIn alloc_cache
dealloc_cache <- function(cache = default_cache(), force = F) {
  if (class(cache) != "workshop_cache") stop("Must pass in a `workshop_cache`!")
  # If alloc level is missing or is > zero
  if ((cache$alloc_level %||% 1) <= 1 || force) {
    cache$alloc_level <<- 0L
    cache$data <<- NULL
    print("dealloc")
  } else {
    cache$alloc_level <<- cache$alloc_level - 1L
  }
}

#' @export
get_target_cache <- function(target, cache = default_cache()) {
  if (class(cache) != "workshop_cache") stop("Must pass in a `workshop_cache`!")
  alloc_cache(cache)
  ret_val <- cache$data$targets[[path_ext_remove(target)]]
  dealloc_cache(cache)
  ret_val
}

#' @export
get_targets_cache <- function(target_spec, cache = default_cache()) {
  if (class(cache) != "workshop_cache") stop("Must pass in a `workshop_cache`!")
  alloc_cache(cache)
  targets <- cache$data$targets
  ret_val <- targets[spec_match(names(targets), path_ext_remove(target_spec))]
  dealloc_cache(cache)
  ret_val
}

upsert_target_cache <- function(target, val, cache = default_cache()) {
  # Force refresh before we write
  alloc_cache(cache, force = T)
  cache$data$targets[[target]] <<- val
  write_yaml(cache$data, cache$path)
  # Alloc level will now be zero, so this is overly conservative
  dealloc_cache(cache)
}
