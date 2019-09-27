#' @export
sch_between <- function(start, end, schedule, inclusive = TRUE) {
  start <- vec_cast_date(start)
  end <- vec_cast_date(end)
  schedule <- as_schedule(schedule)
  vec_assert(inclusive, logical(), 1L)

  init_schedule(schedule)
  context <- get_context(schedule)

  v8_eval(context, "var start = [[as_js_from_date(start)]]")
  v8_eval(context, "var end = [[as_js_from_date(end)]]")
  v8_assign(context, "inclusive", inclusive)

  out <- v8_get(context, "ruleset.between(start, end, inc = inclusive)")
  parse_js_date(out)
}

#' @export
sch_after <- function(x, schedule, inclusive = FALSE) {
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

#' @export
sch_before <- function(x, schedule, inclusive = FALSE) {
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
