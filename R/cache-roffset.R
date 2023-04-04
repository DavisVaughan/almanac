cache_roffset <- R6::R6Class(
  "cache_roffset",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rschedule, offset)
      cache_roffset__initialize(self, private, rschedule, offset),

    get_events = function()
      cache_roffset__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rschedule = NULL,
    offset = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_roffset__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_roffset__cache_build <- function(self, private) {
  rschedule <- private$rschedule
  offset <- private$offset

  # Get the events of `rschedule`
  events <- rschedule_events(rschedule)

  # Adjust them as necessary according to `offset`
  events <- events + offset

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

# ------------------------------------------------------------------------------

cache_roffset__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache_roffset__initialize <- function(self, private, rschedule, offset) {
  private$rschedule <- rschedule
  private$offset <- offset
  self
}
