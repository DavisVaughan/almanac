#' Generate the next or previous event
#'
#' @description
#'
#' - `alma_next()` generates the next event after `x`.
#'
#' - `alma_previous()` generates the previous event before `x`.
#'
#' @inheritParams alma_search
#'
#' @param x `[Date]`
#'
#'    A vector of dates to look forward or backwards from.
#'
#' @export
#' @examples
#' on_12th <- monthly() %>% recur_on_mday(12)
#' on_monday <- weekly() %>% recur_on_wday("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' sch <- schedule() %>%
#'   sch_rrule(on_12th) %>%
#'   sch_rrule(on_monday)
#'
#' alma_next(c("2019-01-01", "2019-01-11"), sch)
#' alma_previous(c("2019-01-01", "2019-01-11"), sch)
alma_next <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  schedule <- as_schedule(schedule)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  events <- schedule$cache$get_events()

  alma_next_impl(x, events, inclusive)
}

alma_next_impl <- function(x, events, inclusive) {
  .Call(export_alma_next_impl, x, events, inclusive)
}

#' @rdname alma_next
#' @export
alma_previous <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  schedule <- as_schedule(schedule)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  events <- schedule$cache$get_events()

  alma_previous_impl(x, events, inclusive)
}

alma_previous_impl <- function(x, events, inclusive) {
  .Call(export_alma_previous_impl, x, events, inclusive)
}
