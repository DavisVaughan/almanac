#' @export
sch_in <- function(x, schedule) {
  x <- vec_cast_date(x)

  min <- min(x)
  max <- max(x)

  events <- sch_between(min, max, schedule)

  vec_in(x, events)
}
