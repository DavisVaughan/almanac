CalendarCache <- R6::R6Class(
  "CalendarCache",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    set_rbundles = function(rbundles)
      calendarcache__set_rbundles(self, private, rbundles),

    set_adjustments = function(adjustments)
      calendarcache__set_adjustments(self, private, adjustments),

    set_weekend = function(weekend)
      calendarcache__set_weekend(self, private, weekend),

    get_events = function()
      calendarcache__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rbundles = list(),
    adjustments = list(),
    weekend = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      calendarcache__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

calendarcache__set_rbundles <- function(self, private, rbundles) {
  private$rbundles <- rbundles
  self
}

calendarcache__set_adjustments <- function(self, private, adjustments) {
  private$adjustments <- adjustments
  self
}

calendarcache__set_weekend <- function(self, private, weekend) {
  private$weekend <- weekend
}

# ------------------------------------------------------------------------------

calendarcache__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

calendarcache__cache_build <- function(self, private) {
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
