cache_radjusted <- R6::R6Class(
  "cache_radjusted",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(cacher, adjuster, adjustment)
      cache_radjusted__initialize(self, private, cacher, adjuster, adjustment),

    get_events = function()
      cache_radjusted__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    cacher = NULL,
    adjuster = NULL,
    adjustment = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_radjusted__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_radjusted__cache_build <- function(self, private) {
  cacher <- private$cacher
  adjuster <- private$adjuster
  adjustment <- private$adjustment

  # Get the events of `cacher`
  events <- cacher_events(cacher)

  # Adjust them as necessary according to `adjuster`
  events <- adjustment(events, adjuster)

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

cache_radjusted__initialize <- function(self, private, cacher, adjuster, adjustment) {
  private$cacher <- cacher
  private$adjuster <- adjuster
  private$adjustment <- adjustment
  self
}
