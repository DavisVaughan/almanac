cache_rsetdiff <- R6::R6Class(
  "cache_rsetdiff",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rschedules, rdates, exdates)
      cache_rsetdiff__initialize(self, private, rschedules, rdates, exdates),

    get_events = function()
      cache_rsetdiff__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rschedules = list(),
    rdates = new_date(),
    exdates = new_date(),

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_rsetdiff__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_rsetdiff__cache_build <- function(self, private) {
  rschedules <- private$rschedules
  rdates <- private$rdates
  exdates <- private$exdates

  # Get events for each rschedule
  rschedules_events <- map(rschedules, rschedule_events)

  # Take the set diff of all of the events from left to right.
  # `vec_set_difference()` ensures uniqueness.
  # `rschedule_events()` ensures sortedness.
  events <- events_set_diff(rschedules_events)

  # Forcibly include `rdates`
  if (!vec_is_empty(rdates)) {
    events <- vec_c(events, rdates)
    events <- vec_sort(events)
  }

  # Forcibly remove `exdates`
  if (!vec_is_empty(exdates)) {
    events <- vec_set_difference(events, exdates)
  }

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

events_set_diff <- function(x) {
  if (!vec_is_list(x)) {
    abort("`x` must be a list.")
  }

  n <- length(x)

  if (n == 0L) {
    out <- new_date()
    return(out)
  }

  if (n == 1L) {
    out <- x[[1]]
    return(out)
  }

  events <- x[[1]]
  x <- x[-1]

  for (i in seq_along(x)) {
    elt <- x[[i]]
    events <- vec_set_difference(events, elt)
  }

  events
}

# ------------------------------------------------------------------------------

cache_rsetdiff__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache_rsetdiff__initialize <- function(self, private, rschedules, rdates, exdates) {
  private$rschedules <- rschedules
  private$rdates <- rdates
  private$exdates <- exdates
  self
}
