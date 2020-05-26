two_names <- c("edd", "eddy")

library(future)
# plan(multisession(workers = 2))

# Hack bc the server is broken
cl <- future::makeClusterPSOCK(
  3L, rscript_libs = c("~/R/x86_64-redhat-linux-gnu-library/3.6", "*"))
plan(cluster, workers = cl)

# If dimension is specified like `name`, then it's broken into smaller targets.
# If dimension is unspecified like `date`, then it's more like dynamic branching behavior.
target("testing/data/batch_files/raw_:name_:date_:size", force_rebuild = T, function(
  size = dimension("big", "small"),
  name = dimension("ed", two_names)#,
  #local_var = dep_local()
) {
  # if (size == "10") {stop("NOOO")}
  Sys.sleep(sample(0:10, 1))
  cat("hii")
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

target("testing/data/flights_by_origin/:origin",
       log_trackables = T,
       force_rebuild = T,
       function(flights = dep_target("testing/data/flights.csv")) {
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

