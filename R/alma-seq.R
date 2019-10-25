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

  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  alma_seq_impl(from, to, schedule, inclusive)
}

alma_seq_impl <- function(from, to, schedule, inclusive = TRUE) {
  if (is.na(from) || is.na(to)) {
    return(global_empty_date)
  }

  cache <- cache_get(schedule, from, to, inclusive)

  if (!is.null(cache)) {
    return(cache)
  }

  init_schedule(schedule)
  since <- sch_since(schedule)

  v8_eval("var from = [[as_js_from_date(since)]]")
  v8_eval("var to = [[as_js_from_date(to)]]")

  # Always set cache with inclusive dates!
  out <- v8_get("ruleset.between(from, to, inc = true)")
  out <- parse_js_date(out)

  cache_set(schedule, to, out)

  if (inclusive) {
    out <- out[out >= from & out <= to]
  } else {
    out <- out[out > from & out < to]
  }

  out
}
