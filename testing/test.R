two_names <- c("edd", "eddy")

# If dimension is specified like `name`, then it's broken into smaller targets.
# If dimension is unspecified like `date`, then it's more like dynamic branching behavior.
target("testing/data/batch_files/raw_:name_:date_:size", function(
  size = dimension("big", "small"),
  name = dimension("ed", two_names)#,
  #local_var = dep_local()
) {
  for (x in 1:3) {
    timer_phase_end("test")
    # y <- read(x) %>% process()
    # Since name is specified, we can't use it as an argument here
    # Since date is unspecified, we must use it as an argument here
    save_target(x, date = x)
  }
})

target(log_trackables = T, "testing/data/flights.csv", function() {
  save_target(nycflights13::flights)
})

target("testing/data/flights_by_origin/:origin", function(flights = dep_target("testing/data/flights.csv")) {
  origins <- unique(flights$origin)
  origins %>% map(function(this_origin) {
    flights %>%
      filter(origin == this_origin) %>%
      save_target(origin = this_origin)
  })
})

target("testing/data/:name_sum", function(
  flights = dep_rollup("testing/data/flights_by_origin/:origin", across = "origin"),
  file = dep_rollup("testing/data/batch_files/raw_:name_:date_big", across = "date")
) {
  str(file)
  str(flights, max.level = 1)
})

