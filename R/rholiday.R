#' Create a recurring holiday
#'
#' @description
#' `rholiday()` is used to create custom holidays. It wraps up a holiday `name`
#' and its corresponding `rschedule` into a holiday object with special
#' properties.
#'
#' Holiday objects can be tweaked with [hol_rename()], [hol_observe()], and
#' [hol_offset()], and they can be added to a calendar with [rcalendar()].
#'
#' @param rschedule `[rschedule]`
#'
#'   The recurrence schedule that determines when the holiday occurs.
#'
#' @param name `[character(1)]`
#'
#'   The name of the holiday. This serves as a unique identifier when adding
#'   multiple holidays to an [rcalendar()].
#'
#' @export
#' @examples
#' on_christmas <- yearly() %>%
#'   recur_on_month_of_year("Dec") %>%
#'   recur_on_day_of_month(25)
#'
#' # Bundle a holiday name with its recurrence schedule to create a holiday
#' rholiday(on_christmas, "Christmas")
#'
#' # This is how the built in holiday objects are created
#' hol_christmas()
rholiday <- function(rschedule, name) {
  check_rschedule(rschedule)
  check_name(name)

  new_rholiday(
    name = name,
    robserved = rschedule,
    runobserved = rschedule
  )
}

new_rholiday <- function(
  name,
  robserved,
  runobserved,
  ...,
  class = character()
) {
  new_rschedule(
    name = name,
    robserved = robserved,
    runobserved = runobserved,
    ...,
    class = c(class, "almanac_rholiday")
  )
}

# ------------------------------------------------------------------------------

#' @export
print.almanac_rholiday <- function(x, ...) {
  cli::cli_text("<{x$name}>")

  cli_indented()
  print(rholiday_robserved(x))
  cli::cli_end()

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.almanac_rholiday <- function(x) {
  x <- rholiday_robserved(x)
  rschedule_events(x)
}

# ------------------------------------------------------------------------------

#' Holiday utility functions
#'
#' @description
#' These three functions allow you to tweak existing holidays created by
#' [rholiday()] so that they more properly align with business calendars. The
#' resulting holidays can then be added into an [rcalendar()].
#'
#' - `hol_observe()` adjusts a holiday based on when it is actually observed.
#'   For example, many holidays that occur on a Saturday are actually observed
#'   on the preceding Friday or following Monday.
#'
#' - `hol_offset()` creates a new holiday by _offsetting_ it from an existing
#'   one. For example, Boxing Day is the day after Christmas, and the observance
#'   of Boxing Day may be dependent on the observance of Christmas (i.e. if
#'   Christmas is Sunday, it may be observed on Monday, so Boxing Day would be
#'   observed on Tuesday).
#'
#' - `hol_rename()` renames an existing holiday. This is typically useful after
#'   a call to `hol_offset()`, since it doesn't rename the holiday but you may
#'   want to give it a different name.
#'
#' @inheritParams radjusted
#' @inheritParams roffset
#'
#' @param x `[rholiday]`
#'
#'   An rholiday.
#'
#' @param name `[character(1)]`
#'
#'   A new name for the holiday.
#'
#' @name holiday-utilities
#' @examples
#'
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' # Christmas, adjusted to nearest Friday or Monday if it falls on a weekend
#' on_christmas <- hol_christmas() %>%
#'   hol_observe(on_weekends, adj_nearest)
#'
#' # Boxing Day is the day after Christmas.
#' # If observed Christmas is a Friday, then observed Boxing Day should be Monday.
#' # If observed Christmas is a Monday, then observed Boxing Day should be Tuesday.
#' on_boxing_day <- on_christmas %>%
#'   hol_offset(1) %>%
#'   hol_observe(on_weekends, adj_following) %>%
#'   hol_rename("Boxing Day")
#'
#' christmas_dates <- alma_events(on_christmas, year = 2010:2015)
#' boxing_day_dates <- alma_events(on_boxing_day, year = 2010:2015)
#'
#' data.frame(
#'   christmas = christmas_dates,
#'   boxing_day = boxing_day_dates,
#'   christmas_weekday = lubridate::wday(christmas_dates, label = TRUE),
#'   boxing_day_weekday = lubridate::wday(boxing_day_dates, label = TRUE)
#' )
NULL

#' @export
#' @rdname holiday-utilities
hol_observe <- function(x, adjust_on, adjustment) {
  check_rholiday(x)

  name <- rholiday_name(x)
  robserved <- rholiday_robserved(x)
  runobserved <- rholiday_runobserved(x)

  robserved <- radjusted(robserved, adjust_on, adjustment)

  new_rholiday(
    name = name,
    robserved = robserved,
    runobserved = runobserved
  )
}

#' @export
#' @rdname holiday-utilities
hol_offset <- function(x, by) {
  check_rholiday(x)

  name <- rholiday_name(x)
  robserved <- rholiday_robserved(x)
  runobserved <- rholiday_runobserved(x)

  robserved <- roffset(robserved, by)
  runobserved <- roffset(runobserved, by)

  new_rholiday(
    name = name,
    robserved = robserved,
    runobserved = runobserved
  )
}

#' @export
#' @rdname holiday-utilities
hol_rename <- function(x, name) {
  check_rholiday(x)
  check_name(name)

  new_rholiday(
    name = name,
    robserved = rholiday_robserved(x),
    runobserved = rholiday_runobserved(x)
  )
}

# ------------------------------------------------------------------------------

check_rholiday <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_inherits(
    x = x,
    what = "almanac_rholiday",
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

list_check_all_rholidays <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  for (i in seq_along(x)) {
    check_rholiday(
      x = x[[i]],
      arg = cli::format_inline("{arg}[[{i}]]"),
      call = call
    )
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

rholiday_robserved <- function(x) {
  x$robserved
}

rholiday_runobserved <- function(x) {
  x$runobserved
}

rholiday_name <- function(x) {
  x$name
}
