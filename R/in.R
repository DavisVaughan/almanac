#' @export
alma_in <- function(x, schedule) {
  x <- vec_cast_date(x)

  min <- min(x)
  max <- max(x)

  events <- alma_between(min, max, schedule)

  vec_in(x, events)
}
