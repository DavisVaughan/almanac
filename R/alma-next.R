#' Generate the next or previous event
#'
#' @description
#'
#' - `alma_next()` generates the next event after `x`.
#'
#' - `alma_previous()` generates the previous event before `x`.
#'
#' @inheritParams adj_following
#'
#' @param inclusive `[logical(1)]`
#'
#'   If `x` is an event, should it be considered the next or previous event?
#'
#' @return
#' A Date vector the same size as `x`.
#'
#' @export
#' @examples
#' on_12th <- monthly() %>% recur_on_day_of_month(12)
#' on_monday <- weekly() %>% recur_on_day_of_week("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' rb <- runion(on_12th, on_monday)
#'
#' alma_next(c("2019-01-01", "2019-01-11"), rb)
#' alma_previous(c("2019-01-01", "2019-01-11"), rb)
alma_next <- function(x, rschedule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  check_bool(inclusive)

  check_rschedule(rschedule)
  events <- rschedule_events(rschedule)

  alma_next_impl(x, events, inclusive)
}

alma_next_impl <- function(x, events, inclusive) {
  .Call(export_alma_next_impl, x, events, inclusive)
}

#' @rdname alma_next
#' @export
alma_previous <- function(x, rschedule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  check_bool(inclusive)

  check_rschedule(rschedule)
  events <- rschedule_events(rschedule)

  alma_previous_impl(x, events, inclusive)
}

alma_previous_impl <- function(x, events, inclusive) {
  .Call(export_alma_previous_impl, x, events, inclusive)
}

# ------------------------------------------------------------------------------

alma_locate_next <- function(x, events, inclusive) {
  .Call(export_alma_locate_next, x, events, inclusive)
}

alma_locate_previous <- function(x, events, inclusive) {
  .Call(export_alma_locate_previous, x, events, inclusive)
}
