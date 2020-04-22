# Built on top of an `rschedule`, this adjusted version will apply
# an adjustment to the events that `rschedule` returns whenever those events
# fall on the events that `adjust_on` returns. This is appropriate
# for representing corporate holidays, where for example, Christmas is normally
# December 25th, unless it falls on a weekend, in which case it is rolled
# to the nearest workday.

# ------------------------------------------------------------------------------

radjusted <- function(rschedule, adjust_on, adjustment) {
  new_radjusted(rschedule, adjust_on, adjustment)
}

#' @export
rschedule_events.radjusted <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

new_radjusted <- function(rschedule, adjust_on, adjustment) {
  validate_rschedule(rschedule, "rschedule")
  validate_rschedule(adjust_on, "adjust_on")
  validate_adjustment(adjustment, "adjustment")

  cache <- cache_radjusted$new(
    rschedule = rschedule,
    adjust_on = adjust_on,
    adjustment = adjustment
  )

  data <- list(
    rschedule = rschedule,
    adjust_on = adjust_on,
    adjustment = adjustment,
    cache = cache
  )

  new_rschedule(data, class = "radjusted")
}
