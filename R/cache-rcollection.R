cache_rcollection <- R6::R6Class(
  "cache_rcollection",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(cachers)
      cache_rcollection__initialize(self, private, cachers),

    get_events = function()
      cache_rcollection__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    cachers = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_rcollection__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_rcollection__cache_build <- function(self, private) {
  cachers <- private$cachers

  # Get events for each cacher
  cachers_events <- map(cachers, cacher_events)

  # Combine, sort, and uniquify
  events <- vec_unchop(cachers_events, ptype = new_date())
  events <- vec_unique(events)
  events <- vec_sort(events)

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

cache_rcollection__initialize <- function(self, private, cachers) {
  private$cachers <- cachers
  self
}
