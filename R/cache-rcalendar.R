cache_rcalendar <- R6::R6Class(
  "cache_rcalendar",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rholidays)
      cache_rcalendar__initialize(self, private, rholidays),

    get_names = function()
      cache_rcalendar__get_names(self, private),

    get_events = function()
      cache_rcalendar__get_events(self, private),

    get_events_frame = function()
      cache_rcalendar__get_events_frame(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rholidays = list(),
    names = NULL,

    events = NULL,
    events_frame = NULL,

    built = FALSE,

    cache_build = function()
      cache_rcalendar__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_rcalendar__cache_build <- function(self, private) {
  rholidays <- private$rholidays
  names <- private$names

  # Get events for each rholidays
  rschedules_events <- map(rholidays, rschedule_events)

  # Build `events_frame` which holds all results, regardless of uniqueness,
  # sorted by event date but ties go to the order in which they were added to
  # the rcalendar
  names <- vec_rep_each(names, times = list_sizes(rschedules_events))
  events <- list_unchop(rschedules_events, ptype = new_date())

  events_frame <- data_frame(name = names, date = events)
  events_frame <- vec_slice(events_frame, vec_order(events))

  private$events_frame <- events_frame

  # Now build `events`, which holds sorted unique results
  events <- vec_unique(events_frame$date)

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

# ------------------------------------------------------------------------------

cache_rcalendar__get_names <- function(self, private) {
  private$names
}

cache_rcalendar__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

cache_rcalendar__get_events_frame <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events_frame
}

# ------------------------------------------------------------------------------

cache_rcalendar__initialize <- function(self, private, rholidays) {
  private$rholidays <- rholidays
  private$names <- map_chr(rholidays, function(rholiday) rholiday$name)
  self
}
