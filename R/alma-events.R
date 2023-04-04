#' Get all events
#'
#' `alma_events()` retrieves all of the events in the rschedule's
#' event set.
#'
#' @inheritParams adj_following
#'
#' @return
#' A Date vector of events.
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
#' rb <- runion() %>%
#'   add_rschedule(rrule) %>%
#'   add_rschedule(rrule_weekly)
#'
#' alma_events(rb)
alma_events <- function(rschedule) {
  check_rschedule(rschedule)
  rschedule_events(rschedule)
}
