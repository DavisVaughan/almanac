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
