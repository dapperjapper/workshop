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

target("data/flights.csv", function() {
  save_target(nycflights13::flights)
})

target("data/flights_by_origin/:origin", function(flights = dep_target("data/flights.csv")) {
  origins <- unique(flights$origin)
  browser()
  origins %>% map(function(this_origin) {
    flights %>%
      filter(origin == this_origin) %>%
      save_target(origin = this_origin)
  })
})


