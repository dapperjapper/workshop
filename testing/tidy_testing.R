library(dplyr)
library(lubridate)

helper <- function(x) {
  x+2
}

pure_method <- purify_function(function(data = dep_local()) {
  helper(6)
  data %>%
    filter(product_type == "CC") %>%
    mutate(open_year = year(open_dt)) %>%
    select(open_year, balance_am)
})

digest::digest(pure_method)
digest::digest(pure_method$trackables)
digest::digest(pure_method$value)