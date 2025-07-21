cache_radjusted <- R6::R6Class(
  "cache_radjusted",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rschedule, adjust_on, adjustment) {
      cache_radjusted__initialize(
        self,
        private,
        rschedule,
        adjust_on,
        adjustment
      )
    },

    get_events = function() {
      cache_radjusted__get_events(self, private)
    }
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rschedule = NULL,
    adjust_on = NULL,
    adjustment = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function() {
      cache_radjusted__cache_build(self, private)
    }
  )
)

# ------------------------------------------------------------------------------

cache_radjusted__cache_build <- function(self, private) {
  rschedule <- private$rschedule
  adjust_on <- private$adjust_on
  adjustment <- private$adjustment

  # Get the events of `rschedule`
  events <- rschedule_events(rschedule)

  # Adjust them as necessary according to `adjust_on`
  events <- adjustment(events, adjust_on)

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

cache_radjusted__initialize <- function(
  self,
  private,
  rschedule,
  adjust_on,
  adjustment
) {
  private$rschedule <- rschedule
  private$adjust_on <- adjust_on
  private$adjustment <- adjustment
  self
}
