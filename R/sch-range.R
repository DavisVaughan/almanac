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
#' on_12th <- monthly() %>% recur_on_mday(12)
#' on_monday <- weekly() %>% recur_on_wday("Monday")
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

  sch_seq_impl(from, to, schedule, inclusive)
}

sch_seq_impl <- function(from, to, schedule, inclusive = TRUE) {
  cache <- cache_get(schedule, from, to, inclusive)

  if (!is.null(cache)) {
    return(cache)
  }

  init_schedule(schedule)

  v8_eval("var from = [[as_js_from_date(from)]]")
  v8_eval("var to = [[as_js_from_date(to)]]")

  # Always set cache with inclusive dates!
  out <- v8_get("ruleset.between(from, to, inc = true)")
  out <- parse_js_date(out)

  cache_set(schedule, from, to, out)

  if (!inclusive) {
    out <- out[out > from & out < to]
  }

  out
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
#' on_12th <- monthly() %>% recur_on_mday(12)
#' on_monday <- weekly() %>% recur_on_wday("Monday")
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

  v8_eval("var x = [[as_js_from_date(x)]]")
  v8_assign("inclusive", inclusive)

  out <- v8_get("ruleset.after(x, inc = inclusive)")
  parse_js_date(out)
}

#' @rdname sch_next
#' @export
sch_previous <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  init_schedule(schedule)

  v8_eval("var x = [[as_js_from_date(x)]]")
  v8_assign("inclusive", inclusive)

  out <- v8_get("ruleset.before(x, inc = inclusive)")
  parse_js_date(out)
}
