cache_radjusted <- R6::R6Class(
  "cache_radjusted",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rschedule, adjustment_rschedule, adjustment)
      cache_radjusted__initialize(self, private, rschedule, adjustment_rschedule, adjustment),

    get_events = function()
      cache_radjusted__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rschedule = NULL,
    adjustment_rschedule = NULL,
    adjustment = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_radjusted__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_radjusted__cache_build <- function(self, private) {
  rschedule <- private$rschedule
  adjustment_rschedule <- private$adjustment_rschedule
  adjustment <- private$adjustment

  # Get the events of `rschedule`
  events <- rschedule_events(rschedule)

  # Adjust them as necessary according to `adjustment_rschedule`
  events <- adjustment(events, adjustment_rschedule)

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

# ------------------------------------------------------------------------------

cache_radjusted__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache_radjusted__initialize <- function(self, private, rschedule, adjustment_rschedule, adjustment) {
  private$rschedule <- rschedule
  private$adjustment_rschedule <- adjustment_rschedule
  private$adjustment <- adjustment
  self
}
