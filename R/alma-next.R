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
#' rb <- rbundle() %>%
#'   add_rrule(on_12th) %>%
#'   add_rrule(on_monday)
#'
#' alma_next(c("2019-01-01", "2019-01-11"), rb)
#' alma_previous(c("2019-01-01", "2019-01-11"), rb)
alma_next <- function(x, rbundle, inclusive = FALSE) {
  x <- vec_cast_date(x)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  validate_cacher(rbundle, "rbundle")
  events <- cacher_events(rbundle)

  alma_next_impl(x, events, inclusive)
}

alma_next_impl <- function(x, events, inclusive) {
  .Call(export_alma_next_impl, x, events, inclusive)
}

#' @rdname alma_next
#' @export
alma_previous <- function(x, rbundle, inclusive = FALSE) {
  x <- vec_cast_date(x)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  validate_cacher(rbundle, "rbundle")
  events <- cacher_events(rbundle)

  alma_previous_impl(x, events, inclusive)
}

alma_previous_impl <- function(x, events, inclusive) {
  .Call(export_alma_previous_impl, x, events, inclusive)
}
