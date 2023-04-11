#' Create a recurring calendar
#'
#' @description
#' `rcalendar()` creates a calendar filled with holidays created from one of the
#' existing `hol_*()` holidays (such as [hol_christmas()]) or from a manually
#' generated holiday created using [rholiday()]. That calendar can then be used
#' as an rschedule with any other `alma_*()` function (like [alma_in()]), or
#' with one of the specialized calendar functions, like [cal_match()] or
#' [cal_events()].
#'
#' @param ... `[rholidays]`
#'
#'   One or more holidays created from [rholiday()] or `hol_*()`.
#'
#' @export
#' @examples
#' on_earth_day <- yearly() %>%
#'   recur_on_month_of_year("April") %>%
#'   recur_on_day_of_month(22) %>%
#'   rholiday("Earth Day")
#'
#' cal <- rcalendar(
#'   hol_christmas(),
#'   on_earth_day,
#'   hol_us_independence_day()
#' )
#'
#' cal
#'
#' cal_events(cal, year = 2020:2022)
#'
#' # Lookup holiday name based on date, if it exists
#' cal_match(c("2021-12-25", "2021-12-26"), cal)
#'
#' # Find next holiday
#' alma_next("2021-12-26", cal)
rcalendar <- function(...) {
  rholidays <- list2(...)
  list_check_all_rholidays(rholidays)

  names <- map_chr(rholidays, rholiday_name)

  if (vec_duplicate_any(names)) {
    loc <- vec_duplicate_detect(names)
    loc <- which(loc)[[1L]]
    name <- names[[loc]]

    cli::cli_abort(c(
      "Can't supply duplicate holidays.",
      i = "The name {.str {name}} is duplicated."
    ))
  }

  new_rcalendar(
    names = names,
    rholidays = rholidays
  )
}

new_rcalendar <- function(names = character(),
                          rholidays = list(),
                          ...,
                          class = character()) {
  check_character(names)
  vec_check_list(rholidays)

  cache <- cache_rcalendar$new(
    names = names,
    rholidays = rholidays
  )

  new_rschedule(
    names = names,
    rholidays = rholidays,
    cache = cache,
    ...,
    class = c(class, "almanac_rcalendar")
  )
}

# ------------------------------------------------------------------------------

#' @export
print.almanac_rcalendar <- function(x, ...) {
  names <- cal_names(x)
  n <- length(names)

  cli::cli_text("<rcalendar[{n}]>")
  cli::cli_ul(names)

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.almanac_rcalendar <- function(x) {
  x$cache$get_events(observed = TRUE)
}

# ------------------------------------------------------------------------------

#' Calendar names
#'
#' `cal_names()` returns the names of the holidays in a calendar.
#'
#' @param x `[rcalendar]`
#'
#'   An rcalendar.
#'
#' @returns
#' A character vector of holiday names.
#'
#' @export
#' @examples
#' x <- rcalendar(hol_christmas(), hol_new_years_day())
#' cal_names(x)
cal_names <- function(x) {
  check_rcalendar(x)
  x$names
}

# ------------------------------------------------------------------------------

#' Calendar events
#'
#' @description
#' `cal_events()` returns a data frame of holiday name / event date pairs for
#' a calendar. It is similar to [alma_events()], but returns information about
#' the name of the holiday and has specialized behavior related to observed
#' dates when filtering by `year`.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[rcalendar]`
#'
#'   An rcalendar.
#'
#' @param year `[integer]`
#'
#'   An integer vector of years to filter for.
#'
#' @param observed `[FALSE / TRUE]`
#'
#'   When filtering for specific `year`s, should the _observed_ date of the
#'   holiday be used for filtering purposes? If `FALSE`, the _actual_ date of
#'   the holiday will be used, i.e. the date before any observance adjustments
#'   created by [hol_observe()] have been applied, which is typically desired
#'   when filtering for a year's worth of holidays. See the examples.
#'
#' @returns
#' A two column data frame:
#' - `name` is a character vector of holiday names.
#' - `date` is a Date vector of holiday event dates.
#'
#' @export
#' @examples
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' # New Year's Day, observed on the nearest weekday if it falls on a weekend
#' on_new_years <- hol_new_years_day() %>%
#'   hol_observe(on_weekends, adj_nearest)
#'
#' # Christmas, observed on the nearest weekday if it falls on a weekend
#' on_christmas <- hol_christmas() %>%
#'   hol_observe(on_weekends, adj_nearest)
#'
#' cal <- rcalendar(on_new_years, on_christmas)
#' cal
#'
#' # In 2010, Christmas fell on a Saturday and was adjusted backwards
#' cal_events(cal, year = 2010)
#'
#' # In 2011, New Year's fell on a Saturday and was adjusted backwards.
#' # Note that the returned date is in 2010, even though we requested holidays
#' # for 2011, because most people would consider the actual New Year's date of
#' # 2011-01-01 part of the 2011 set of holidays, even though it was observed in
#' # 2010.
#' cal_events(cal, year = 2011)
#'
#' # If you want to filter by the observed date, set `observed = TRUE`, which
#' # will move the New Year's Day that was observed in 2010 to the 2010 result
#' cal_events(cal, year = 2010, observed = TRUE)
#' cal_events(cal, year = 2011, observed = TRUE)
cal_events <- function(x, ..., year = NULL, observed = FALSE) {
  check_dots_empty0(...)
  check_rcalendar(x)
  check_bool(observed)

  out <- cal_events_frame(x, observed = TRUE)

  if (is_null(year)) {
    return(out)
  }

  year <- vec_cast(year, to = integer())
  check_no_missing(year)

  if (observed) {
    years <- date_year(out$date)
  } else {
    # Filter using the original year before any adjustments have been made
    # (i.e. so a holiday that occurred in 2023 but was observed in 2024 will
    # show up in the 2023 list)
    out_unobserved <- cal_events_frame(x, observed = FALSE)
    years <- date_year(out_unobserved$date)
  }

  out <- vec_slice(out, vec_in(years, year))

  out
}

# ------------------------------------------------------------------------------

#' Calendar matching
#'
#' @description
#' `cal_match()` matches a date in `x` to a holiday in `rcalendar` and returns
#' the corresponding holiday name, or `NA` if it doesn't exist in the calendar.
#'
#' If a date corresponds to multiple holidays, the holiday that was added to the
#' calendar first is returned.
#'
#' This function is intended to be similar to [base::match()].
#'
#' @param x `[Date]`
#'
#'   A date vector to match.
#'
#' @param rcalendar `[rcalendar]`
#'
#'   A calendar to look for holiday matches in.
#'
#' @returns
#' A character vector the same size as `x`.
#'
#' @export
#' @examples
#' cal <- rcalendar(
#'   hol_christmas(),
#'   hol_halloween(),
#'   hol_new_years_day(),
#'   hol_us_presidents_day()
#' )
#'
#' x <- as.Date(c(
#'   "2019-01-02",
#'   "2019-12-25",
#'   "2018-02-19",
#'   "2018-02-20",
#'   "2020-10-31"
#' ))
#'
#' cal_match(x, cal)
cal_match <- function(x, rcalendar) {
  x <- vec_cast_date(x)
  check_rcalendar(rcalendar)

  frame <- cal_events(rcalendar)

  loc <- vec_match(x, frame$date)
  out <- vec_slice(frame$name, loc)

  out
}

# ------------------------------------------------------------------------------

#' Calendar additions and removals
#'
#' @description
#' - `cal_add()` adds an rholiday to an rcalendar.
#'
#' - `cal_remove()` removes an rholiday from an rcalendar by name, either by
#'   specifying a character name or an rholiday object with the same name.
#'
#' @param x `[rcalendar]`
#'
#'   An rcalendar.
#'
#' @param rholiday `[rholiday]`
#'
#'   An rholiday to add to the rcalendar.
#'
#' @param what `[character(1) / rholiday]`
#'
#'   The name of a holiday to remove from the rcalendar, or an rholiday object
#'   with the corresponding name that you'd like to remove.
#'
#' @returns
#' A new rcalendar with the holiday added or removed.
#'
#' @name calendar-add-remove
#' @examples
#' cal <- rcalendar(
#'   hol_christmas(),
#'   hol_halloween(),
#'   hol_new_years_day(),
#'   hol_us_presidents_day()
#' )
#'
#' # Can't forget Easter!
#' cal %>%
#'   cal_add(hol_easter())
#'
#' # Didn't actually need Halloween
#' cal %>%
#'   cal_remove(hol_halloween())
#'
#' # Can remove by name or by object
#' cal %>%
#'   cal_remove("Halloween")
NULL

#' @export
#' @rdname calendar-add-remove
cal_add <- function(x, rholiday) {
  check_rcalendar(x)
  check_rholiday(rholiday)

  names <- cal_names(x)
  name <- rholiday_name(rholiday)

  if (name %in% names) {
    cli::cli_abort(c(
      "Can't add a holiday that already exists in the calendar.",
      i = "{.str {name}} already exists in the calendar."
    ))
  }

  # Add to the end
  names <- c(names, name)

  rholidays <- cal_rholidays(x)
  rholidays <- c(rholidays, list(rholiday))

  new_rcalendar(
    names = names,
    rholidays = rholidays
  )
}

#' @export
#' @rdname calendar-add-remove
cal_remove <- function(x, what) {
  check_rcalendar(x)

  if (is_character(what)) {
    check_name(what)
  } else {
    check_rholiday(what)
    what <- rholiday_name(what)
  }

  names <- cal_names(x)
  loc <- vec_match(what, names)

  if (is.na(loc)) {
    cli::cli_abort(c(
      "Can't remove a holiday that isn't in the calendar.",
      i = "{.str {what}} isn't in the calendar."
    ))
  }

  rholidays <- cal_rholidays(x)

  # Drop and rebuild
  names <- names[-loc]
  rholidays <- rholidays[-loc]

  new_rcalendar(
    names = names,
    rholidays = rholidays
  )
}

# ------------------------------------------------------------------------------

#' Calendar locations
#'
#' @description
#' - `cal_next()` generates the next holiday after `x`.
#'
#' - `cal_previous()` generates the previous holiday before `x`.
#'
#' If no holiday exists before/after `x`, a missing row is generated.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams alma_next
#'
#' @param rcalendar `[rcalendar]`
#'
#'   An rcalendar.
#'
#' @returns
#' A two column data frame, like `cal_events()`, which is the same size as `x`
#' and contains either the next or previous holiday relative to `x`.
#'
#' @name calendar-locations
#' @examples
#' x <- as.Date(c("2023-04-11", "2023-08-10", "2021-05-06"))
#' cal <- cal_us_federal()
#'
#' cal_next(x, cal)
#' cal_previous(x, cal)
NULL

#' @export
#' @rdname calendar-locations
cal_next <- function(x, rcalendar, ..., inclusive = FALSE) {
  check_dots_empty0(...)
  x <- vec_cast_date(x)
  check_rcalendar(rcalendar)
  check_bool(inclusive)

  frame <- cal_events(rcalendar)
  events <- frame$date

  loc <- alma_locate_next(x = x, events = events, inclusive = inclusive)

  vec_slice(frame, loc)
}

#' @export
#' @rdname calendar-locations
cal_previous <- function(x, rcalendar, ..., inclusive = FALSE) {
  check_dots_empty0(...)
  x <- vec_cast_date(x)
  check_rcalendar(rcalendar)
  check_bool(inclusive)

  frame <- cal_events(rcalendar)
  events <- frame$date

  loc <- alma_locate_previous(x = x, events = events, inclusive = inclusive)

  vec_slice(frame, loc)
}

# ------------------------------------------------------------------------------

cal_events_frame <- function(x, ..., observed = TRUE) {
  check_dots_empty0(...)
  x$cache$get_events_frame(observed)
}

cal_rholidays <- function(x) {
  x$rholidays
}

# ------------------------------------------------------------------------------

check_rcalendar <- function(x,
                            ...,
                            allow_null = FALSE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  check_inherits(
    x = x,
    what = "almanac_rcalendar",
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
