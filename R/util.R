#' @importFrom glue glue_collapse
commas <- function(...) {
  glue_collapse(..., sep = ", ", last = " and ")
}
