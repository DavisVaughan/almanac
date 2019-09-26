alma_between <- function(start, end, rrule) {
  start <- vec_cast_date(start)
  end <- vec_cast_date(end)

  init_rrule(rrule)
  context <- get_context(rrule)

  v8_eval(context, "var start = [[as_js_from_date(start)]]")
  v8_eval(context, "var end = [[as_js_from_date(end)]]")

  out <- v8_get(context, "rule.between(start, end, inc = true)")
  parse_js_date(out)
}

alma_after <- function(x, rrule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  init_rrule(rrule)
  context <- get_context(rrule)

  v8_eval(context, "var x = [[as_js_from_date(x)]]")
  v8_assign(context, "inclusive", inclusive)

  out <- v8_get(context, "rule.after(x, inc = inclusive)")
  parse_js_date(out)
}

alma_before <- function(x, rrule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  init_rrule(rrule)
  context <- get_context(rrule)

  v8_eval(context, "var x = [[as_js_from_date(x)]]")
  v8_assign(context, "inclusive", inclusive)

  out <- v8_get(context, "rule.before(x, inc = inclusive)")
  parse_js_date(out)
}
