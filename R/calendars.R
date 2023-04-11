#' US federal calendar
#'
#' @description
#' `cal_us_federal()` is an example calendar that represents the federal
#' holidays in the United States. It makes no attempt to be historically
#' accurate, but instead represents the _currently_ recognized federal holidays.
#' The calendar represents the _observed_ dates of each holiday, rather than the
#' actual dates of each holiday (i.e. if a holiday falls on a Saturday, it is
#' federally observed on the preceding Friday).
#'
#' Refer to the source code of `cal_us_federal()` to get a feel for how to
#' build your own personal calendar.
#'
#' @inheritParams hol_christmas
#'
#' @returns
#' An rcalendar.
#'
#' @export
#' @examples
#' cal <- cal_us_federal()
#'
#' # All 2023 holidays
#' cal_events(cal, year = 2023)
#'
#' # Notice that for 2028, `cal_events()` knows that you probably want to
#' # treat New Year's Day as a 2028 holiday even though it will observed in
#' # 2027 (because it will be a Saturday and will be rolled back to being
#' # observed on Friday)
#' cal_events(cal, year = 2028)
#'
#' # Were any of these dates on a holiday?
#' x <- as.Date(c(
#'   "2023-11-10",
#'   "2023-10-05",
#'   "2023-06-19",
#'   "2023-05-29",
#'   "2023-05-28"
#' ))
#'
#' alma_in(x, cal)
#'
#' # Which one?
#' cal_match(x, cal)
cal_us_federal <- function(since = NULL, until = NULL) {
  on_weekends <- weekly(since = since, until = until)
  on_weekends <- recur_on_weekends(on_weekends)

  rcalendar(
    hol_new_years_day(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_martin_luther_king_junior_day(since = since, until = until),
    hol_us_presidents_day(since = since, until = until),
    hol_us_memorial_day(since = since, until = until),
    hol_us_juneteenth(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_independence_day(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_labor_day(since = since, until = until),
    hol_us_indigenous_peoples_day(since = since, until = until),
    hol_us_veterans_day(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_thanksgiving(since = since, until = until),
    hol_christmas(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest)
  )
}

# ------------------------------------------------------------------------------

# Internal Posit calendar
cal_posit <- function(since = NULL, until = NULL) {
  on_weekends <- weekly(since = since, until = until)
  on_weekends <- recur_on_weekends(on_weekends)

  rcalendar(
    hol_new_years_day(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_martin_luther_king_junior_day(since = since, until = until),
    hol_us_presidents_day(since = since, until = until),
    hol_massachusetts_patriots_day(since = since, until = until),
    hol_us_memorial_day(since = since, until = until),
    hol_us_juneteenth(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_independence_day(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_labor_day(since = since, until = until),
    hol_us_indigenous_peoples_day(since = since, until = until),
    hol_us_election_day(since = since, until = until) %>%
      hol_rename("US Citizenship Day"),
    hol_us_veterans_day(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_thanksgiving(since = since, until = until),
    # Day after Thanksgiving will always be a Friday, so no observance needed
    hol_us_thanksgiving(since = since, until = until) %>%
      hol_offset(by = 1) %>%
      hol_rename("US Day after Thanksgiving"),
    hol_christmas(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
    # For Day after Christmas:
    # - Observe Christmas on nearest workday
    # - Then offset that forward one day
    # - Then observe day after Christmas on following workday (always adjust
    #   forward in case Christmas was on a Friday, in which case the day after
    #   is Saturday, but observed on Monday)
    hol_christmas(since = since, until = until) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_nearest) %>%
      hol_offset(by = 1L) %>%
      hol_observe(adjust_on = on_weekends, adjustment = adj_following) %>%
      hol_rename("Day after Christmas")
  )
}

hol_massachusetts_patriots_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = since)
  out <- recur_on_month_of_year(out, "April")
  out <- recur_on_day_of_week(out, "Monday", nth = 3L)

  rholiday(
    rschedule = out,
    name = "Massachusetts Patriots' Day"
  )
}
