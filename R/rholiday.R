new_rholiday <- function(name,
                         rschedule,
                         ...,
                         class = character()) {
  check_name(name)
  validate_rschedule(rschedule, "rschedule")

  new_rschedule(
    name = name,
    rschedule = rschedule,
    ...,
    class = c(class, "rholiday")
  )
}

#' @export
print.rholiday <- function(x, ...) {
  cat_line(glue("<{x$name}>"))
  print(x$rschedule)
  invisible(x)
}

#' @export
rschedule_events.rholiday <- function(x) {
  rschedule_events(x$rschedule)
}

hol_christmas <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "December")
  out <- recur_on_mday(out, 25L)

  new_rholiday(
    name = "Christmas",
    rschedule = out
  )
}

hol_christmas_eve <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "December")
  out <- recur_on_mday(out, 24L)

  new_rholiday(
    name = "Christmas Eve",
    rschedule = out
  )
}

hol_easter <- function() {
  out <- yearly()
  out <- recur_on_easter(out)

  new_rholiday(
    name = "Easter",
    rschedule = out
  )
}

hol_good_friday <- function() {
  out <- yearly()
  out <- recur_on_easter(out, offset = -2L)

  new_rholiday(
    name = "Good Friday",
    rschedule = out
  )
}

hol_halloween <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "October")
  out <- recur_on_mday(out, 31L)

  new_rholiday(
    name = "Halloween",
    rschedule = out
  )
}

hol_new_years_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "January")
  out <- recur_on_mday(out, 1L)

  new_rholiday(
    name = "New Year's Day",
    rschedule = out
  )
}

hol_new_years_eve <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "December")
  out <- recur_on_mday(out, 31L)

  new_rholiday(
    name = "New Year's Eve",
    rschedule = out
  )
}

hol_passover <- function() {
  out <- yearly()
  out <- recur_on_easter(out, offset = -3L)

  new_rholiday(
    name = "Passover",
    rschedule = out
  )
}

hol_st_patricks_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "March")
  out <- recur_on_mday(out, 17L)

  new_rholiday(
    name = "Saint Patrick's Day",
    rschedule = out
  )
}

hol_valentines_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "February")
  out <- recur_on_mday(out, 14L)

  new_rholiday(
    name = "Valentine's Day",
    rschedule = out
  )
}

hol_us_election_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "November")
  out <- recur_on_wday(out, "Tuesday")
  out <- recur_on_mday(out, 2:8)

  new_rholiday(
    name = "US Election Day",
    rschedule = out
  )
}

hol_us_fathers_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "June")
  out <- recur_on_wday(out, "Sunday", nth = 3L)

  new_rholiday(
    name = "US Father's Day",
    rschedule = out
  )
}

hol_us_independence_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "July")
  out <- recur_on_mday(out, 4L)

  new_rholiday(
    name = "US Independence Day",
    rschedule = out
  )
}

hol_us_indigenous_peoples_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "October")
  out <- recur_on_wday(out, "Monday", nth = 2L)

  new_rholiday(
    name = "US Indigenous Peoples' Day",
    rschedule = out
  )
}

hol_us_juneteenth <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "June")
  out <- recur_on_mday(out, 19L)

  new_rholiday(
    name = "US Juneteenth",
    rschedule = out
  )
}

hol_us_labor_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "September")
  out <- recur_on_wday(out, "Monday", nth = 1L)

  new_rholiday(
    name = "US Labor Day",
    rschedule = out
  )
}

hol_us_martin_luther_king_junior_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "January")
  out <- recur_on_wday(out, "Monday", nth = 3L)

  new_rholiday(
    name = "US Martin Luther King Jr. Day",
    rschedule = out
  )
}

hol_us_memorial_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "May")
  out <- recur_on_wday(out, "Monday", nth = -1L)

  new_rholiday(
    name = "US Memorial Day",
    rschedule = out
  )
}

hol_us_mothers_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "May")
  out <- recur_on_wday(out, "Sunday", nth = 2L)

  new_rholiday(
    name = "US Mother's Day",
    rschedule = out
  )
}

hol_us_presidents_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "February")
  out <- recur_on_wday(out, "Monday", nth = 3L)

  new_rholiday(
    name = "US Presidents' Day",
    rschedule = out
  )
}

hol_us_thanksgiving <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "November")
  out <- recur_on_wday(out, "Thursday", nth = 4L)

  new_rholiday(
    name = "US Thanksgiving",
    rschedule = out
  )
}

hol_us_veterans_day <- function() {
  out <- yearly()
  out <- recur_on_ymonth(out, "November")
  out <- recur_on_mday(out, 11L)

  new_rholiday(
    name = "US Veterans Day",
    rschedule = out
  )
}

hol_offset <- function(x, offset) {
  rschedule <- roffset(
    rschedule = hol_rschedule(x),
    offset = offset
  )

  new_rholiday(name = hol_name(x), rschedule = rschedule)
}

hol_adjust <- function(x, adjust_on, adjustment) {
  rschedule <- radjusted(
    rschedule = hol_rschedule(x),
    adjust_on = adjust_on,
    adjustment = adjustment
  )

  new_rholiday(name = hol_name(x), rschedule = rschedule)
}

hol_rename <- function(x, name) {
  check_name(name)
  new_rholiday(name = name, rschedule = hol_rschedule(x))
}

hol_name <- function(x) {
  x$name
}
hol_rschedule <- function(x) {
  x$rschedule
}
