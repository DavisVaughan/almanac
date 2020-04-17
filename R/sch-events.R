#' Get all events
#'
#' `sch_events()` retrieves all of the events defined by the schedule's
#' recurrence rules.
#'
#' @param schedule `[schedule / rrule]`
#'
#'   A schedule or rrule.
#'
#' @return A `Date` vector of events.
#'
#' @export
#' @examples
#' rrule <- daily(since = "1970-01-01", until = "1970-01-05")
#'
#' sch_events(rrule)
#'
#' rrule_weekly <- weekly(since = "1970-01-01") %>%
#'   recur_for_count(5)
#'
#' sch <- schedule() %>%
#'   sch_rrule(rrule) %>%
#'   sch_rrule(rrule_weekly)
#'
#' sch_events(sch)
sch_events <- function(schedule) {
  schedule <- as_schedule(schedule)
  schedule$cache$get()
}
