cache_calendar <- R6::R6Class(
  "cache_calendar",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rbundles, adjustments, weekend)
      cache_calendar__initialize(self, private, rbundles, adjustments, weekend),

    get_events = function()
      cache_calendar__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rbundles = list(),
    adjustments = list(),
    weekend = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_calendar__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_calendar__initialize <- function(self, private, rbundles, adjustments, weekend) {
  private$rbundles <- rbundles
  private$adjustments <- adjustments
  private$weekend <- weekend
  self
}

# ------------------------------------------------------------------------------

cache_calendar__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache_calendar__cache_build <- function(self, private) {
  rbundles <- private$rbundles
  adjustments <- private$adjustments
  weekend <- private$weekend

  rbundles_events <- map(rbundles, rbundle_events)

  rbundles_events <- map2(
    rbundles_events,
    adjustments,
    apply_adjustment,
    weekend = weekend
  )

  events <- vec_unchop(rbundles_events, ptype = new_date())
  events <- vec_sort(events)
  events <- vec_unique(events)

  private$events <- events
  private$built <- TRUE

  self
}

rbundle_events <- function(rbundle) {
  rbundle$cache$get_events()
}

apply_adjustment <- function(events, adjustment, weekend) {
  adjustment(events, weekend)
}
