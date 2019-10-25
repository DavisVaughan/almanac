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
  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  init_schedule(schedule)

  v8_eval("var x = [[as_js_from_date(x)]]")
  v8_assign("inclusive", inclusive)

  out <- v8_get("ruleset.after(x, inc = inclusive)")
  parse_js_date(out)
}

#' @rdname alma_next
#' @export
alma_previous <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  init_schedule(schedule)

  v8_eval("var x = [[as_js_from_date(x)]]")
  v8_assign("inclusive", inclusive)

  out <- v8_get("ruleset.before(x, inc = inclusive)")
  parse_js_date(out)
}
