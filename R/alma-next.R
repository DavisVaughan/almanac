#' Generate the next or previous event
#'
#' @description
#'
#' - `alma_next()` generates the next event after `x`.
#'
#' - `alma_previous()` generates the previous event before `x`.
#'
#' @inheritParams alma_seq
#'
#' @param x `[Date(1)]`
#'
#'    The date to start the search from.
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
#' alma_next("2019-01-01", sch)
#'
#' alma_previous("2019-01-01", sch)
#'
#' @export
alma_next <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)
  vec_assert(x, size = 1L)

  if (is.na(x)) {
    abort("`x` cannot be `NA`")
  }

  schedule <- as_schedule(schedule)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  alma_next_impl(x, schedule, inclusive)
}

alma_next_impl <- function(x, schedule, inclusive) {
  cache <- schedule$cache

  out <- cache$slice_next(x, inclusive)

  if (!is.null(out)) {
    return(out)
  }

  recurrences <- schedule$recurrences
  cache$cache_next(recurrences, x, inclusive)

  cache$slice_next(x, inclusive)
}

#' @rdname alma_next
#' @export
alma_previous <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)
  vec_assert(x, size = 1L)

  if (is.na(x)) {
    abort("`x` cannot be `NA`")
  }

  schedule <- as_schedule(schedule)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  alma_previous_impl(x, schedule, inclusive)
}

alma_previous_impl <- function(x, schedule, inclusive) {
  cache <- schedule$cache

  out <- cache$slice_previous(x, inclusive)

  if (!is.null(out)) {
    return(out)
  }

  recurrences <- schedule$recurrences
  cache$cache_previous(recurrences, x, inclusive)

  cache$slice_previous(x, inclusive)
}
