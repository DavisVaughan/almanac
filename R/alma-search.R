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
#' @export
#' @examples
#' on_12th <- monthly() %>% recur_on_mday(12)
#' on_monday <- weekly() %>% recur_on_wday("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' rb <- rbundle() %>%
#'   add_rschedule(on_12th) %>%
#'   add_rschedule(on_monday)
#'
#' alma_search("2019-01-01", "2019-01-31", rb)
alma_search <- function(from, to, rschedule, inclusive = TRUE) {
  from <- vec_cast_date(from)
  to <- vec_cast_date(to)

  vec_assert(from, size = 1L)
  vec_assert(to, size = 1L)

  if (is.na(from) || is.na(to)) {
    abort("`from` and `to` cannot be `NA`")
  }

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  validate_rschedule(rschedule, "rschedule")
  events <- rschedule_events(rschedule)

  alma_search_impl(events, from, to, inclusive)
}

alma_search_impl <- function(events, from, to, inclusive) {
  .Call(export_alma_search_impl, events, from, to, inclusive)
}
