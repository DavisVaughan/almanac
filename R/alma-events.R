#' Get all events
#'
#' `alma_events()` retrieves all of the events in the rschedule's
#' event set.
#'
#' @inheritParams adj_following
#' @inheritParams rlang::args_dots_empty
#'
#' @param year `[NULL / integer]`
#'
#'   An optional integer vector of years to limit the returned events to.
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
#' on_christmas <- yearly() %>%
#'   recur_on_month_of_year("Dec") %>%
#'   recur_on_day_of_month(25)
#'
#' alma_events(on_christmas, year = c(2020, 2022))
alma_events <- function(rschedule, ..., year = NULL) {
  check_dots_empty0(...)
  check_rschedule(rschedule)

  out <- rschedule_events(rschedule)

  if (is_null(year)) {
    return(out)
  }

  year <- vec_cast(year, to = integer())
  check_no_missing(year)

  years <- date_year(out)
  out <- vec_slice(out, vec_in(years, year))

  out
}

date_year <- function(x) {
  x <- as.POSIXlt.Date(x)
  x <- x$year + 1900L
  x
}
