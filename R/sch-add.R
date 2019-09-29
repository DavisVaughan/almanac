#' @export
sch_add_rrule <- function(x, rrule) {
  validate_schedule(x)
  validate_rrule(rrule, arg = "`rrule`")
  x$recurrences$rrules <- c(x$recurrences$rrules, list(rrule))
  x
}

#' @export
sch_add_rdate <- function(x, rdate) {
  validate_schedule(x)
  rdate <- vec_cast_date(rdate, "rdate")
  x$recurrences$rdates <- c(x$recurrences$rdates, list(rdate))
  x
}

#' @export
sch_add_exdate <- function(x, exdate) {
  validate_schedule(x)
  exdate <- vec_cast_date(exdate, "exdate")
  x$recurrences$exdates <- c(x$recurrences$exdates, list(exdate))
  x
}

#' @export
sch_add_schedule <- function(x, schedule) {
  validate_schedule(x)
  validate_schedule(schedule, "`schedule`")

  x_recurrences <- x$recurrences
  y_recurrences <- schedule$recurrences

  new_schedule(
    rrules = c(x_recurrences$rrules, y_recurrences$rrules),
    rdates = c(x_recurrences$rdates, y_recurrences$rdates),
    exdates = c(x_recurrences$exdates, y_recurrences$exdates)
  )
}
