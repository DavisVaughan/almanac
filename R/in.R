alma_in <- function(x, rrule) {
  x <- vec_cast_date(x)

  min <- min(x)
  max <- max(x)

  events <- alma_between(min, max, rrule)

  vec_in(x, events)
}
