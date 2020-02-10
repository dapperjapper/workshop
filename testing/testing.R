local_var <- 4
x <- "no"

helper <- function(x, y) {
  print("helper")
  round(x + y)
}

impure_helper <- function(y, x) {
  local_var + x
}

local({
  helperhelper <- function () {
    2
  }
  helper2 <- function(x, y) {
    print("helper")
    round(helper(x, y) + helperhelper())
  }
  purify_function(helper2)
})

target("data/raw_data", function() {
  print(exists("local_var"))
  print(exists("x"))
  return(tibble(hey = "now"))
})

target("data/analysis", function(
  # Creates dependency structure -- loan_target from within method is outlawed
  raw_data = dep_target("data/raw_data"),
  # Explicit demarkation of dependencies from global env
  local_var = dep_local()
) {
  print(exists("local_var")) # yes
  print(exists("x")) # no, not specified
  print(local_var + 8)
  test <- 8 + local_var
  # Need any external functions used (that are not from packages) to be specifically
  # called out?
  helper(test, local_var)
  return(length(raw_data) + local_var)
})

batch_targets("data/batch_files/:name", function(...) {
  for (x in long_list) {
    y <- read(x) %>% process()
    save_target(y, name = x)
  }
})

#' PURITY
#'
#' What is allowed inside of target methods:
#' - Explicitly specified dependencies
#'   - Objects                                Invalidated through inequality
#'   - File paths                             Invalidated through file modified dates
#'   - Other targets                          Invalidated recursively
#'   - Dirty functions from devtools shim /   Must specify cache invalidation method
#'     method source environment
#' - Functions from packages                  Invalidated through package versioning
#' - Pure functions from devtools shim /      Invalidated through recursive code analysis
#'   method source environment
#'
#' What is not allowed inside of target methods:
#' - Unspecified non-function objects from method source environment
#' - Loading from unspecified file paths
#' - Other targets
