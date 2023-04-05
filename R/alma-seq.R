#' Generate date sequences
#'
#' `alma_seq()` generates a sequence of all dates between `from` and `to`,
#' skipping any events defined by the `rschedule`.
#'
#' @inheritParams alma_search
#'
#' @param from,to `[Date(1)]`
#'
#'    Dates defining the range to look for events.
#'
#' @param inclusive `[logical(1)]`
#'
#'    If `from` or `to` are events in the `rschedule`, should they be removed
#'    from the sequence?
#'
#' @return
#' A vector of dates in the range of `[from, to]`, with all events in the
#' `rschedule` removed.
#'
#' @export
#' @examples
#' on_weekends <- weekly() %>% recur_on_weekends()
#'
#' # Generate a sequence of all non-weekend dates in Jan-2000
#' alma_seq("2000-01-01", "2000-01-31", on_weekends)
alma_seq <- function(from, to, rschedule, inclusive = TRUE) {
  from <- vec_cast_date(from)
  to <- vec_cast_date(to)

  vec_check_size(from, size = 1L)
  vec_check_size(to, size = 1L)

  check_no_missing(from)
  check_no_missing(to)

  dates <- date_seq2(from, to)
  events <- alma_search(from, to, rschedule, inclusive = inclusive)

  vec_set_difference(dates, events)
}

date_seq2 <- function(from, to) {
  # Avoid `seq.Date()` error if `from > to`. Use better `seq2()`.
  from <- unclass(from)
  to <- unclass(to)

  out <- seq2(from, to)
  out <- as.double(out)

  new_date(out)
}
