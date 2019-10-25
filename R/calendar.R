#' Calendars
#'
#' The following generate `schedule` objects corresponding to calendars. A
#' calendar is composed of multiple holiday schedules generated from the
#' `hldy_*()` functions.
#'
#' @param since `[Date(1)]`
#'
#'    The initial date to start the schedule from.
#'
#' @examples
#' on_us_holidays <- calendar_usa_federal()
#'
#' # Locate holidays
#' sch_seq("2000-01-01", "2001-01-01", on_us_holidays)
#'
#' # Adjust dates relative to holidays
#' on_weekends <- recur_on_weekends(weekly())
#' on_us_holidays_and_weekends <- sch_rrule(on_us_holidays, on_weekends)
#'
#' x <- as.Date("2019-08-30")
#' sch_step(x, 1, sch_rrule(on_us_holidays, on_weekends))
#'
#' @name calendars
NULL

# ------------------------------------------------------------------------------

#' @rdname calendars
#' @export
calendar_usa_federal <- function(since = "1970-01-01") {
  since <- vec_cast_date(since, "since")

  sch <- schedule()

  sch <- sch_merge(sch, hldy_washington_birthday(since))
  sch <- sch_merge(sch, hldy_memorial_day(since))
  sch <- sch_merge(sch, hldy_labor_day(since))
  sch <- sch_merge(sch, hldy_columbus_day(since))
  sch <- sch_merge(sch, hldy_veterans_day(since))
  sch <- sch_merge(sch, hldy_new_years_day(since))
  sch <- sch_merge(sch, hldy_martin_luther_king_jr_day(since))
  sch <- sch_merge(sch, hldy_independence_day(since))
  sch <- sch_merge(sch, hldy_thanksgiving(since))
  sch <- sch_merge(sch, hldy_christmas(since))

  sch
}

# ------------------------------------------------------------------------------

# http://s3.amazonaws.com/armstrongeconomics-wp/2013/07/NYSE-Closings.pdf

#' @rdname calendars
#' @export
calendar_usa_nyse <- function(since = "1970-01-01") {
  since <- vec_cast_date(since, "since")

  # Only since 1885-01-01
  since <- max(as.Date("1885-01-01"), since)

  sch <- schedule()

  sch <- sch_merge(sch, hldy_new_years_day(since))

  nyse_start_mlk <- max(as.Date("1998-01-01"), since)
  sch <- sch_merge(sch, hldy_martin_luther_king_jr_day(nyse_start_mlk))

  nyse_stop_lincoln <- as.Date("1953-02-12")
  if (since <= nyse_stop_lincoln) {
    nyse_start_lincoln <- max(as.Date("1896-02-12"), since)
    sch_lincoln <- hldy_lincoln_birthday(nyse_start_lincoln, nyse_stop_lincoln)
    sch <- sch_merge(sch, sch_lincoln)
  }

  sch <- sch_merge(sch, hldy_washington_birthday(since))

  sch <- add_nyse_good_friday(sch, since)

  nyse_start_memorial_day <- max(as.Date("1873-01-01"), since)
  sch <- sch_merge(sch, hldy_memorial_day(nyse_start_memorial_day))

  sch <- sch_merge(sch, hldy_independence_day(since))

  nyse_start_labor_day <- max(as.Date("1887-01-01"), since)
  sch <- sch_merge(sch, hldy_labor_day(nyse_start_labor_day))

  nyse_stop_columbus <- as.Date("1953-10-12")
  if (since <= nyse_stop_columbus) {
    nyse_start_columbus <- max(as.Date("1909-10-12"), since)
    sch_columbus <- hldy_columbus_day(nyse_start_columbus, nyse_stop_columbus)
    sch <- sch_merge(sch, sch_columbus)
  }

  sch <- add_nyse_election_day(sch, since)

  # TODO - Veteran's Day


  sch <- sch_merge(sch, hldy_thanksgiving(since))

  sch <- sch_merge(sch, hldy_christmas(since))

  sch
}

# All years except 1898, 1906, 1907
add_nyse_good_friday <- function(sch, since) {
  sch_merge(sch, nyse_good_friday(since))
}

nyse_good_friday <- function(since) {
  if (since > as.Date("1907-03-29")) {
    return(hldy_good_friday(since))
  }

  if (since >= as.Date("1906-04-13")) {
    return(hldy_good_friday(as.Date("1907-03-30")))
  }

  if (since >= as.Date("1898-04-08")) {
    sch <- hldy_good_friday(since = as.Date("1898-04-09"), until = as.Date("1906-04-12"))
    sch <- sch_merge(sch, hldy_good_friday(as.Date("1907-03-30")))
    return(sch)
  }

  sch <- hldy_good_friday(since = since, until = as.Date("1898-04-07"))
  sch <- sch_merge(sch, hldy_good_friday(since = as.Date("1898-04-09"), until = as.Date("1906-04-12")))
  sch <- sch_merge(sch, hldy_good_friday(as.Date("1907-03-30")))
  sch
}

add_nyse_election_day <- function(sch, since) {
  if (since > as.Date("1980-11-04")) {
    return(sch)
  }

  sch_merge(sch, nyse_election_day(since))
}

nyse_election_day <- function(since) {
  election_1980 <- as.Date("1980-11-04")
  election_1976 <- as.Date("1976-11-02")

  if (since > election_1976) {
    return(hldy_election_day(since = election_1980, until = election_1980))
  }

  election_1972 <- as.Date("1972-11-07")

  if (since > election_1972) {
    return(hldy_election_day(since = election_1976, until = election_1976))
  }

  election_1968 <- as.Date("1968-11-05")

  if (since > election_1968) {
    return(hldy_election_day(since = election_1972, until = election_1972))
  }

  hldy_election_day(since = since, until = election_1968)
}
