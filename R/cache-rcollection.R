cache_rcollection <- R6::R6Class(
  "cache_rcollection",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(cachers = list(), rdates = new_date(), exdates = new_date())
      cache_rcollection__initialize(self, private, cachers, rdates, exdates),

    get_events = function()
      cache_rcollection__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    cachers = NULL,
    rdates = NULL,
    exdates = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_rcollection__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_rcollection__cache_build <- function(self, private) {
  cachers <- private$cachers
  rdates <- private$rdates
  exdates <- private$exdates

  # Get events for each cacher
  cachers_events <- map(cachers, cacher_events)

  # Forcibly include `rdates`
  if (!vec_is_empty(rdates)) {
    cachers_events <- c(cachers_events, list(rdates))
  }

  # Combine, sort, and uniquify
  events <- vec_unchop(cachers_events, ptype = new_date())
  events <- vec_unique(events)
  events <- vec_sort(events)

  # Forcibly remove `exdates`
  if (!vec_is_empty(exdates)) {
    events <- vec_set_diff(events, exdates)
  }

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

# ------------------------------------------------------------------------------

cache_rcollection__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache_rcollection__initialize <- function(self, private, cachers, rdates, exdates) {
  private$cachers <- cachers
  private$rdates <- rdates
  private$exdates <- exdates
  self
}
