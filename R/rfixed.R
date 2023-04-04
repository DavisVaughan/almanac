new_rfixed <- function(events, ..., class = character()) {
  # check_date(x, allow_na = FALSE, allow_infinite = FALSE)
  new_rschedule(events = events, ..., class = c(class, "rfixed"))
}

rfixed <- function(events) {
  events <- vec_cast_date(events, "events")
  events <- vec_unique(events)
  events <- vec_sort(events)
  new_rfixed(events)
}

#' @export
print.rfixed <- function(x, ...) {
  cat_line(format(x))
  invisible(x)
}

#' @export
format.rfixed <- function(x, ...) {
  events <- x$events
  n_events <- length(events)

  if (n_events > 5L) {
    events <- vec_slice(events, 1:5)
    extra <- glue("* and {n_events - 5L} more")
  } else {
    extra <- character()
  }

  header <- glue("<rfixed[{n_events}]>")

  out <- vec_paste0("* ", events)
  out <- c(header, out, extra)
  out <- glue::glue_collapse(out, sep = "\n")
  out <- as.character(out)

  out
}

#' @export
rschedule_events.rfixed <- function(x) {
  x$events
}
