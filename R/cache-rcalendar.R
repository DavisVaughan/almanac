cache_rcalendar <- R6::R6Class(
  "cache_rcalendar",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(names, rholidays)
      cache_rcalendar__initialize(self, private, names, rholidays),

    get_events = function(observed)
      cache_rcalendar__get_events(self, private, observed),

    get_events_frame = function(observed)
      cache_rcalendar__get_events_frame(self, private, observed)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    names = character(),
    rholidays = list(),

    observed = list(events = NULL, events_frame = NULL, built = FALSE),
    unobserved = list(events = NULL, events_frame = NULL, built = FALSE),

    cache_build = function(observed)
      cache_rcalendar__cache_build(self, private, observed)
  )
)

# ------------------------------------------------------------------------------

cache_rcalendar__cache_build <- function(self, private, observed) {
  names <- private$names
  rholidays <- private$rholidays

  # Which cache are we building?
  if (observed) {
    rholiday_rschedule <- rholiday_robserved
  } else {
    rholiday_rschedule <- rholiday_runobserved
  }

  # Get events for each rholidays
  rschedules <- map(rholidays, rholiday_rschedule)
  rschedules_events <- map(rschedules, rschedule_events)

  # Build `events_frame` which holds all results, regardless of uniqueness,
  # sorted by event date but ties go to the order in which they were added to
  # the rcalendar
  names <- vec_rep_each(names, times = list_sizes(rschedules_events))
  events <- list_unchop(rschedules_events, ptype = new_date())

  events_frame <- data_frame(name = names, date = events)
  events_frame <- vec_slice(events_frame, vec_order(events))

  # Now build `events`, which holds sorted unique results
  events <- vec_unique(events_frame$date)

  result <- list(
    events = events,
    events_frame = events_frame,
    built = TRUE
  )

  if (observed) {
    private$observed <- result
  } else {
    private$unobserved <- result
  }

  invisible(self)
}

# ------------------------------------------------------------------------------

cache_rcalendar__get_events <- function(self, private, observed) {
  if (observed) {
    built <- private$observed$built
  } else {
    built <- private$unobserved$built
  }

  if (!built) {
    private$cache_build(observed = observed)
  }

  if (observed) {
    private$observed$events
  } else {
    private$unobserved$events
  }
}

cache_rcalendar__get_events_frame <- function(self, private, observed) {
  if (observed) {
    built <- private$observed$built
  } else {
    built <- private$unobserved$built
  }

  if (!built) {
    private$cache_build(observed = observed)
  }

  if (observed) {
    private$observed$events_frame
  } else {
    private$unobserved$events_frame
  }
}

# ------------------------------------------------------------------------------

cache_rcalendar__initialize <- function(self, private, names, rholidays) {
  private$names <- names
  private$rholidays <- rholidays
  self
}
