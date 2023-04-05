#' Search for events
#'
#' `alma_search()` retrieves all events between `from` and `to`.
#'
#' @inheritParams adj_following
#'
#' @param from,to `[Date(1)]`
#'
#'    Dates defining the range to look for events.
#'
#' @param inclusive `[logical(1)]`
#'
#'    If `from` or `to` are events, should they be included?
#'
#' @return
#' A Date vector of all events between `from` and `to`.
#'
#' @export
#' @examples
#' on_12th <- monthly() %>% recur_on_day_of_month(12)
#' on_monday <- weekly() %>% recur_on_day_of_week("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' rb <- runion() %>%
#'   add_rschedule(on_12th) %>%
#'   add_rschedule(on_monday)
#'
#' alma_search("2019-01-01", "2019-01-31", rb)
alma_search <- function(from, to, rschedule, inclusive = TRUE) {
  from <- vec_cast_date(from)
  to <- vec_cast_date(to)

  vec_check_size(from, size = 1L)
  vec_check_size(to, size = 1L)

  check_no_missing(from)
  check_no_missing(to)

  check_bool(inclusive)

  check_rschedule(rschedule)
  events <- rschedule_events(rschedule)

  alma_search_impl(events, from, to, inclusive)
}

alma_search_impl <- function(events, from, to, inclusive) {
  .Call(export_alma_search_impl, events, from, to, inclusive)
}
