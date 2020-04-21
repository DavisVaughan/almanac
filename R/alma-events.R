#' Get all events
#'
#' `alma_events()` retrieves all of the events in the rbundle's recurrence set.
#'
#' @param rbundle `[rbundle / rrule]`
#'
#'   An rbundle or rrule.
#'
#' @return A `Date` vector of events.
#'
#' @export
#' @examples
#' rrule <- daily(since = "1970-01-01", until = "1970-01-05")
#'
#' alma_events(rrule)
#'
#' rrule_weekly <- weekly(since = "1970-01-01") %>%
#'   recur_for_count(5)
#'
#' rb <- rbundle() %>%
#'   add_rrule(rrule) %>%
#'   add_rrule(rrule_weekly)
#'
#' alma_events(rb)
alma_events <- function(rbundle) {
  validate_cacher(rbundle, "rbundle")
  cacher_events(rbundle)
}
