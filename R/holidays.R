#' Holidays
#'
#' The following generate `schedule` objects corresponding to commonly used
#' holidays.
#'
#' @param since `[Date(1)]`
#'
#'    The initial date to start the schedule from.
#'
#' @param until `[NULL / Date(1)]`
#'
#'    An optional end date for the holiday schedule. Useful when constructing
#'    larger calendars, such as one for the NYSE, where holidays are
#'    occasionally only celebrated for a set number of years.
#'
#' @examples
#' on_thanksgiving <- hldy_thanksgiving()
#'
#' sch_seq("2000-01-01", "2019-01-01", on_thanksgiving)
#'
#' @name holidays
NULL

# ------------------------------------------------------------------------------
# >= 1971 = Third Monday in February
# <  1971 = - Feb 22nd if weekday
#           - Feb 23 if that was Monday
#           - Feb 21 if that was Friday

#' @rdname holidays
#' @export
hldy_washington_birthday <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  if (since >= as.Date("1971-01-01")) {
    hldy_washington_birthday_post_1971(since)
  } else {
    sch <- schedule()
    sch <- sch_add_schedule(sch, hldy_washington_birthday_pre_1971(since))
    sch <- sch_add_schedule(sch, hldy_washington_birthday_post_1971())
    sch
  }
}

hldy_washington_birthday_pre_1971 <- function(since) {
  until <- as.Date("1970-12-31")

  # On Feb 22 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_until(rrule, until)
  rrule <- recur_on_ymonth(rrule, 2L)
  rrule <- recur_on_mday(rrule, 22L)
  rrule <- recur_on_weekdays(rrule)

  # On Feb 23 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_until(rrule_adj_forward, until)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 2L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 23L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On Feb 21 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_until(rrule_adj_backward, until)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 2L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 21L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

hldy_washington_birthday_post_1971 <- function(since = as.Date("1971-01-01")) {
  # On third Monday in February
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 2L)
  rrule <- recur_on_wday(rrule, 1L, nth = 3L)
  as_schedule(rrule)
}

# ------------------------------------------------------------------------------
# >= 1971 = Last Monday in May
# <  1971 = - May 30 if weekday
#           - May 31 if that was Monday
#           - Feb 29 if that was Friday

#' @rdname holidays
#' @export
hldy_memorial_day <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  if (since >= as.Date("1971-01-01")) {
    hldy_memorial_day_post_1971(since)
  } else {
    sch <- schedule()
    sch <- sch_add_schedule(sch, hldy_memorial_day_pre_1971(since))
    sch <- sch_add_schedule(sch, hldy_memorial_day_post_1971())
    sch
  }
}

hldy_memorial_day_pre_1971 <- function(since) {
  until <- as.Date("1970-12-31")

  # On May 30 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_until(rrule, until)
  rrule <- recur_on_ymonth(rrule, 5L)
  rrule <- recur_on_mday(rrule, 30L)
  rrule <- recur_on_weekdays(rrule)

  # On May 31 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_until(rrule_adj_forward, until)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 5L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 31L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On May 29 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_until(rrule_adj_backward, until)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 5L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 29L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

# On last Monday in May
hldy_memorial_day_post_1971 <- function(since = as.Date("1971-01-01")) {
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 5L)
  rrule <- recur_on_wday(rrule, 1L, nth = -1L)
  as_schedule(rrule)
}

# ------------------------------------------------------------------------------
# On first Monday in September

#' @rdname holidays
#' @export
hldy_labor_day <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 9L)
  rrule <- recur_on_wday(rrule, 1L, nth = 1L)
  as_schedule(rrule)
}

# ------------------------------------------------------------------------------
# >= 1971 = Second Monday in October
# <  1971 = - Oct 12 if weekday
#           - Oct 13 if Oct 12 was Sunday
#           - Oct 11 if Oct 12 was Saturday

#' @rdname holidays
#' @export
hldy_columbus_day <- function(since = "1970-01-01", until = NULL) {
  since <- vec_cast_date(since)

  if (since >= as.Date("1971-01-01")) {
    return(hldy_columbus_day_post_1971(since, until))
  }

  if (is.null(until)) {
    sch <- schedule()
    sch <- sch_add_schedule(sch, hldy_columbus_day_pre_1971(since))
    sch <- sch_add_schedule(sch, hldy_columbus_day_post_1971())
    return(sch)
  }

  if (until >= as.Date("1971-01-01")) {
    sch <- schedule()
    sch <- sch_add_schedule(sch, hldy_columbus_day_pre_1971(since))
    sch <- sch_add_schedule(sch, hldy_columbus_day_post_1971(until = until))
    return(sch)
  }

  hldy_columbus_day_pre_1971(since, until)
}

hldy_columbus_day_pre_1971 <- function(since, until = NULL) {
  if (is.null(until)) {
    until <- as.Date("1970-12-31")
  }

  # On Oct 12 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_until(rrule, until)
  rrule <- recur_on_ymonth(rrule, 10L)
  rrule <- recur_on_mday(rrule, 12L)
  rrule <- recur_on_weekdays(rrule)

  # On Oct 13 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_until(rrule_adj_forward, until)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 10L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 13L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On Oct 11 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_until(rrule_adj_backward, until)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 10L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 11L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

hldy_columbus_day_post_1971 <- function(since = as.Date("1971-01-01"), until = NULL) {
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 10L)
  rrule <- recur_on_wday(rrule, 1L, nth = 2L)

  if (!is.null(until)) {
    rrule <- recur_until(rrule, until)
  }

  as_schedule(rrule)
}

# ------------------------------------------------------------------------------
# >= 1978, <= 1970 = - Nov 11 if weekday
#                    - Nov 12 if that was Monday
#                    - Nov 10 if that was Friday

# 1971 - 1977 = Fourth Monday in October

#' @rdname holidays
#' @export
hldy_veterans_day <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  if (since >= as.Date("1978-01-01")) {
    return(hldy_veterans_day_post_1978(since))
  }

  if (since >= as.Date("1971-01-01")) {
    sch <- schedule()
    sch <- sch_add_schedule(sch, hldy_veterans_day_post_1971_pre_1978(since))
    sch <- sch_add_schedule(sch, hldy_veterans_day_post_1978())
    return(sch)
  }

  sch <- schedule()
  sch <- sch_add_schedule(sch, hldy_veterans_day_pre_1971(since))
  sch <- sch_add_schedule(sch, hldy_veterans_day_post_1971_pre_1978())
  sch <- sch_add_schedule(sch, hldy_veterans_day_post_1978())
  return(sch)
}

hldy_veterans_day_pre_1971 <- function(since) {
  until <- as.Date("1970-12-31")

  # On Nov 11 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_until(rrule, until)
  rrule <- recur_on_ymonth(rrule, 11L)
  rrule <- recur_on_mday(rrule, 11L)
  rrule <- recur_on_weekdays(rrule)

  # On Nov 12 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_until(rrule_adj_forward, until)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 11L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 12L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On Nov 10 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_until(rrule_adj_backward, until)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 11L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 10L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

# Fourth Monday in October
hldy_veterans_day_post_1971_pre_1978 <- function(since = as.Date("1971-01-01")) {
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 11L)
  rrule <- recur_on_wday(rrule, 1L, nth = 4L)
  as_schedule(rrule)
}

# On last Monday in May
hldy_veterans_day_post_1978 <- function(since = as.Date("1978-01-01")) {
  # On Nov 11 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 11L)
  rrule <- recur_on_mday(rrule, 11L)
  rrule <- recur_on_weekdays(rrule)

  # On Nov 12 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 11L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 12L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On Nov 10 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 11L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 10L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

# ------------------------------------------------------------------------------
# - Jan 1 if weekday
# - Jan 2 if Jan 1 was Sunday
# - Dec 31 if Jan 1 was Saturday

#' @rdname holidays
#' @export
hldy_new_years_day <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  # On Jan 1 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 1L)
  rrule <- recur_on_mday(rrule, 1L)
  rrule <- recur_on_wday(rrule, 1:5)

  # On Jan 2 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 1L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 2L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On Dec 31 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 12L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 31L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

# ------------------------------------------------------------------------------
# Third Monday in January after or on 1986

# https://www.calendar-12.com/holidays/martin_luther_king_day/
# "The bill established the holiday was signed by the President Ronald Reagan
# on November 2, 1983. The first observance nationwide was in 1986, but some
# states opposed to observed it as a paid holiday for state employees. As of
# the year 2000 all of the states officially recognize the holiday."

# ^ Using 1986 start date

#' @rdname holidays
#' @export
hldy_martin_luther_king_jr_day <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  hldy_start <- as.Date("1986-01-01")
  if (since < hldy_start) {
    since <- hldy_start
  }

  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 1L)
  rrule <- recur_on_wday(rrule, 1L, nth = 3L)
  as_schedule(rrule)
}

# ------------------------------------------------------------------------------
# - July 4 if weekday
# - July 5 if July 4 was Sunday
# - July 3 if July 4 was Saturday

#' @rdname holidays
#' @export
hldy_independence_day <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  # On July 4 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 7L)
  rrule <- recur_on_mday(rrule, 4L)
  rrule <- recur_on_wday(rrule, 1:5)

  # On July 5 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 7L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 5L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On July 3 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 7L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 3L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

# ------------------------------------------------------------------------------
# - < 1939 Last Thursday in November
# - 1939, 1940, 1941 Next to last Thursday in November
# - > 1941 4th Thursday in November

# https://www.archives.gov/legislative/features/thanksgiving

#' @rdname holidays
#' @export
hldy_thanksgiving <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  if (since >= as.Date("1942-01-01")) {
    return(hldy_thanksgiving_post_1942(since))
  }

  if (since >= as.Date("1939-01-01")) {
    sch <- schedule()
    sch <- sch_add_schedule(sch, hldy_thanksgiving_post_1939_pre_1942(since))
    sch <- sch_add_schedule(sch, hldy_thanksgiving_post_1942())
    return(sch)
  }

  sch <- schedule()
  sch <- sch_add_schedule(sch, hldy_thanksgiving_pre_1939(since))
  sch <- sch_add_schedule(sch, hldy_thanksgiving_post_1939_pre_1942())
  sch <- sch_add_schedule(sch, hldy_thanksgiving_post_1942())
  return(sch)
}

hldy_thanksgiving_pre_1939 <- function(since) {
  until <- as.Date("1938-12-31")

  rrule <- yearly(since)
  rrule <- recur_until(rrule, until)
  rrule <- recur_on_ymonth(rrule, 11L)
  rrule <- recur_on_wday(rrule, 4L, nth = -1L)
  as_schedule(rrule)
}

hldy_thanksgiving_post_1939_pre_1942 <- function(since = as.Date("1939-01-01")) {
  until <- as.Date("1941-12-31")

  rrule <- yearly(since)
  rrule <- recur_until(rrule, until)
  rrule <- recur_on_ymonth(rrule, 11L)
  rrule <- recur_on_wday(rrule, 4L, nth = -2L)
  as_schedule(rrule)
}

hldy_thanksgiving_post_1942 <- function(since = as.Date("1942-01-01")) {
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 11L)
  rrule <- recur_on_wday(rrule, 4L, nth = 4L)
  as_schedule(rrule)
}

# ------------------------------------------------------------------------------
# - Dec 25 if weekday
# - Dec 26 if Dec 25 was Sunday
# - Dec 24 if Dec 25 was Saturday

#' @rdname holidays
#' @export
hldy_christmas <- function(since = "1970-01-01") {
  since <- vec_cast_date(since)

  # On July 4 that is a weekday
  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 12L)
  rrule <- recur_on_mday(rrule, 25L)
  rrule <- recur_on_wday(rrule, 1:5)

  # On July 5 that is a Monday
  rrule_adj_forward <- yearly(since)
  rrule_adj_forward <- recur_on_ymonth(rrule_adj_forward, 12L)
  rrule_adj_forward <- recur_on_mday(rrule_adj_forward, 26L)
  rrule_adj_forward <- recur_on_wday(rrule_adj_forward, 1L)

  # On July 3 that is a Friday
  rrule_adj_backward <- yearly(since)
  rrule_adj_backward <- recur_on_ymonth(rrule_adj_backward, 12L)
  rrule_adj_backward <- recur_on_mday(rrule_adj_backward, 24L)
  rrule_adj_backward <- recur_on_wday(rrule_adj_backward, 5L)

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule = rrule)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_forward)
  sch <- sch_add_rrule(sch, rrule = rrule_adj_backward)

  sch
}

# ------------------------------------------------------------------------------

#' @rdname holidays
#' @export
hldy_easter <- function(since = "1970-01-01", until = NULL) {
  hldy_easter_impl(since = since, until = until)
}

hldy_easter_impl <- function(since, until, offset = 0L) {
  rrule <- yearly(since)
  rrule <- recur_on_easter(rrule, offset = offset)

  if (!is.null(until)) {
    rrule <- recur_until(rrule, until)
  }

  sch <- schedule()
  sch <- sch_add_rrule(sch, rrule)

  sch
}

# ------------------------------------------------------------------------------

#' @rdname holidays
#' @export
hldy_good_friday <- function(since = "1970-01-01", until = NULL) {
  hldy_easter_impl(since = since, until = until, offset = -2L)
}

# ------------------------------------------------------------------------------

#' @rdname holidays
#' @export
hldy_easter_monday <- function(since = "1970-01-01", until = NULL) {
  hldy_easter_impl(since = since, until = until, offset = 1L)
}

# ------------------------------------------------------------------------------
# Since 1870

#' @rdname holidays
#' @export
hldy_july_fourth <- function(since = "1970-01-01") {
  since <- vec_cast_date(since, "since")

  hldy_start <- as.Date("1870-07-04")
  if (since < hldy_start) {
    since <- hldy_start
  }

  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 7L)
  rrule <- recur_on_mday(rrule, 4L)

  sch <- as_schedule(rrule)

  sch
}

# ------------------------------------------------------------------------------
# Used in the NYSE for a limited period

#' @rdname holidays
#' @export
hldy_lincoln_birthday <- function(since = "1970-01-01", until = NULL) {
  since <- vec_cast_date(since, "since")

  rrule <- yearly(since)
  rrule <- recur_on_ymonth(rrule, 2L)
  rrule <- recur_on_mday(rrule, 12L)

  if (!is.null(until)) {
    rrule <- recur_until(rrule, until)
  }

  sch <- as_schedule(rrule)

  sch
}

# ------------------------------------------------------------------------------
# Used in the NYSE for a limited period
# First Tuesday after the first Monday in Nov,
# aka the first Tuesday after Nov 1

#' @rdname holidays
#' @export
hldy_election_day <- function(since = "1970-01-01", until = NULL) {
  since <- vec_cast_date(since, "since")

  hldy_start <- as.Date("1845-01-01")
  if (since <= hldy_start) {
    since <- hldy_start
  }

  rrule <- yearly(since)
  rrule <- recur_on_wday(rrule, 2L)
  rrule <- recur_on_mday(rrule, 2:8)
  rrule <- recur_on_ymonth(rrule, 11L)

  if (!is.null(until)) {
    rrule <- recur_until(rrule, until)
  }

  as_schedule(rrule)
}
