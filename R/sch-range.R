#' Generate events in a schedule
#'
#' `sch_seq()` generates all events in a schedule from `from` to `to`.
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
#' library(magrittr)
#'
#' on_12th <- monthly() %>% rr_on_mday(12)
#' on_monday <- weekly() %>% rr_on_wday("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' sch <- schedule() %>%
#'   sch_add_rrule(on_12th) %>%
#'   sch_add_rrule(on_monday)
#'
#' sch_seq("2019-01-01", "2019-01-31", sch)
#'
#' @export
sch_seq <- function(from, to, schedule, inclusive = TRUE) {
  from <- vec_cast_date(from)
  to <- vec_cast_date(to)

  vec_assert(from, size = 1L)
  vec_assert(to, size = 1L)

  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  init_schedule(schedule)
  context <- get_context(schedule)

  v8_eval(context, "var from = [[as_js_from_date(from)]]")
  v8_eval(context, "var to = [[as_js_from_date(to)]]")
  v8_assign(context, "inclusive", inclusive)

  out <- v8_get(context, "ruleset.between(from, to, inc = inclusive)")
  parse_js_date(out)
}

#' Generate the next or previous event
#'
#' @description
#'
#' - `sch_next()` generates the next event after `x`.
#'
#' - `sch_previous()` generates the previous event before `x`.
#'
#' @inheritParams sch_seq
#'
#' @param x `[Date(1)]`
#'
#'    The date to start the search from.
#'
#' @examples
#' library(magrittr)
#'
#' on_12th <- monthly() %>% rr_on_mday(12)
#' on_monday <- weekly() %>% rr_on_wday("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' sch <- schedule() %>%
#'   sch_add_rrule(on_12th) %>%
#'   sch_add_rrule(on_monday)
#'
#' sch_next("2019-01-01", sch)
#'
#' sch_previous("2019-01-01", sch)
#'
#' @export
sch_next <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  init_schedule(schedule)
  context <- get_context(schedule)

  v8_eval(context, "var x = [[as_js_from_date(x)]]")
  v8_assign(context, "inclusive", inclusive)

  out <- v8_get(context, "ruleset.after(x, inc = inclusive)")
  parse_js_date(out)
}

#' @rdname sch_next
#' @export
sch_previous <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  init_schedule(schedule)
  context <- get_context(schedule)

  v8_eval(context, "var x = [[as_js_from_date(x)]]")
  v8_assign(context, "inclusive", inclusive)

  out <- v8_get(context, "ruleset.before(x, inc = inclusive)")
  parse_js_date(out)
}
