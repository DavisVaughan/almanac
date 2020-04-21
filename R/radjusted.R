# Built on top of an `rschedule`, this adjusted version will apply
# an adjustment to the events that `rschedule` returns whenever those events
# fall on the events that `adjustment_rschedule` returns. This is appropriate
# for representing corporate holidays, where for example, Christmas is normally
# December 25th, unless it falls on a weekend, in which case it is rolled
# to the nearest workday.

# ------------------------------------------------------------------------------

radjusted <- function(rschedule, adjustment_rschedule, adjustment) {
  new_radjusted(rschedule, adjustment_rschedule, adjustment)
}

# ------------------------------------------------------------------------------

new_radjusted <- function(rschedule, adjustment_rschedule, adjustment) {
  validate_rschedule(rschedule, "rschedule")
  validate_rschedule(adjustment_rschedule, "adjustment_rschedule")
  validate_adjustment(adjustment, "adjustment")

  cache <- cache_radjusted$new(
    rschedule = rschedule,
    adjustment_rschedule = adjustment_rschedule,
    adjustment = adjustment
  )

  data <- list(
    rschedule = rschedule,
    adjustment_rschedule = adjustment_rschedule,
    adjustment = adjustment,
    cache = cache
  )

  new_rschedule(data, class = "radjusted")
}
