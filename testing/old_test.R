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
  # Creates dependency structure -- load_target from within method is outlawed
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

# If dimension is specified like `name`, then it's broken into smaller targets.
# If dimension is unspecified like `date`, then it's more like dynamic branching behavior.
target("data/batch_files/raw-:name-:date-:size", function(
  size = dimension("big", "small"),
  name = dimension("ed", "edd", "eddy")#,
  #local_var = dep_local()
) {
  for (x in 1:3) {
    # y <- read(x) %>% process()
    # Since name is specified, we can't use it as an argument here
    # Since date is unspecified, we must use it as an argument here
    save_target(x, date = x)
  }
})

batch_targets("data/batch_files/processed-:name-:date", function(
  raw = dep_target("data/batch_files/raw-:name-:date")
) {
  x <- process(raw)
  # Both dimensions are specified according to the dep_target spec
  save_target(x)
})


