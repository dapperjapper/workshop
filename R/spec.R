# TODO: standard spec class format

#' @importFrom stringr str_match_all str_sub
spec_dimensions <- function(spec) {
  spec <- spec[[1]]

  str_match_all(spec, ":[A-Za-z0-9-]+")[[1]] %>%
    # Trim off colon
    str_sub(2, -1)
}

# #' @importFrom glue glue
#' @importFrom purrr reduce
#' @importFrom stringr str_replace_all str_c
encode_spec <- function(data, spec, allow_missing = F) {
  spec <- spec[[1]]
  spec_dim <- spec_dimensions(spec)
  # data <- c(name = "haha", date = "234-34")
  # spec <- "data/batch_files/raw-:name-:date"

  if (any(str_detect(data, "[^A-Za-z0-9-]"))) {
    stop("Dimension values must only use [A-Za-z0-9-]! Sorry!")
  }
  if (length(setdiff(spec_dim, names(data))) && !allow_missing) {
    stop("Can't encode spec ", spec, " without dimension(s) ", commas(setdiff(spec_dim, names(data))))
  }

  # Only select dimensions that are in the spec
  # This prevents insertion like data[["date-:size"]] to replace multiple dimensions at once
  data <- data[intersect(spec_dim, names(data))] %>% map(as.character)

  # Do the replacement
  purrr::reduce2(data, names(data), .init = spec, function(path, value, var) {
    str_replace_all(path, str_c(":", var), value)
  })
}

spec_to_regex <- function(spec) {
  spec %>%
    str_replace_all(":[A-Za-z0-9-]+", "([A-Za-z0-9-]+)") %>%
    str_c("^", ., "$")
}

#' Vectorized over path! :^)
#' path <- c("data/one_blue", "data/two_green.rds", "data/something/else")
#' spec <- "data/:first_:second"
spec_match <- function(path, spec) {
  if (!length(path))
    return(logical(0))

  spec_regex <- spec[[1]] %>%
    path_ext_remove() %>%
    spec_to_regex()

  path %>%
    path_ext_remove() %>%
    str_detect(spec_regex)
}

#' @importFrom stringr str_match
#' @importFrom tibble enframe deframe
#' @importFrom dplyr distinct count filter pull
decode_spec <- function(path, spec) {
  spec <- path_ext_remove(spec[[1]])
  path <- path_ext_remove(path[[1]])

  spec_regex <- spec_to_regex(spec)
  data_tibble <- path %>%
    str_match(spec_regex) %>%
    # discard the first match, which is the entire string
    .[,-1] %>%
    set_names(spec_dimensions(spec)) %>%
    enframe() %>%
    distinct()

  if (any(is.na(data_tibble$value))) {
    cat("Spec:", spec, "\n")
    cat("Path:", path, "\n")
    failed_dims <- data_tibble %>%
      filter(is.na(value)) %>%
      pull(name)
    stop("Failed to locate dimension(s) in path: ",
         commas(failed_dims))
  }

  data_inconsistencies <- data_tibble %>%
    count(name) %>%
    filter(n > 1)

  if (nrow(data_inconsistencies) > 0) {
    cat("Spec:", spec, "\n")
    cat("Path:", path, "\n")
    stop("Inconsistent dimension(s) in path: ",
         commas(data_inconsistencies$name))
  }

  data_tibble %>% deframe()
}
