#' Generate events in a schedule
#'
#' `alma_seq()` generates all events in a schedule from `from` to `to`.
#'
#' @param from,to `[Date(1)]`
#'
#'    Dates defining the range to look for events in the `schedule`.
#'
#' @param schedule `[schedule / rrule]`
#'
#'    A schedule or rrule.
#'
#' @param inclusive `[logical(1)]`
#'
#'    If `from` or `to` are events, should they be included?
#'
#' @examples
#' on_12th <- monthly() %>% recur_on_mday(12)
#' on_monday <- weekly() %>% recur_on_wday("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' sch <- schedule() %>%
#'   sch_rrule(on_12th) %>%
#'   sch_rrule(on_monday)
#'
#' alma_seq("2019-01-01", "2019-01-31", sch)
#'
#' @export
alma_seq <- function(from, to, schedule, inclusive = TRUE) {
  from <- vec_cast_date(from)
  to <- vec_cast_date(to)

  vec_assert(from, size = 1L)
  vec_assert(to, size = 1L)

  if (is.na(from) || is.na(to)) {
    abort("`from` and `to` cannot be `NA`")
  }

  schedule <- as_schedule(schedule)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  occurrences <- schedule$cache$get()

  alma_seq_impl(occurrences, from, to, inclusive)
}

alma_seq_impl <- function(occurrences, from, to, inclusive) {
  .Call(export_alma_seq_impl, occurrences, from, to, inclusive)
}
