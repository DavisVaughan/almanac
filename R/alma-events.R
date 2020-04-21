#' Get all events
#'
#' `alma_events()` retrieves all of the events in the rschedule's
#' recurrence set.
#'
#' @inheritParams adj_following
#'
#' @return
#' A `Date` vector of events.
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
#' rsched <- rschedule() %>%
#'   add_cacher(rrule) %>%
#'   add_cacher(rrule_weekly)
#'
#' alma_events(rsched)
alma_events <- function(rschedule) {
  validate_cacher(rschedule, "rschedule")
  cacher_events(rschedule)
}
