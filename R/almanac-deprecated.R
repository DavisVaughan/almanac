# ------------------------------------------------------------------------------

#' Deprecated recurrence helpers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' - `recur_on_mday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_month()].
#'
#' - `recur_on_wday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_week()].
#'
#' - `recur_on_yday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_year()].
#'
#' - `recur_on_yweek()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_week_of_year()].
#'
#' - `recur_on_ymonth()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_month_of_year()].
#'
#' @inheritParams recur_on_day_of_month
#' @inheritParams recur_on_day_of_week
#'
#' @param mday `[integer]`
#'
#'    The days of the month on which to recur. Negative values are allowed,
#'    which specify `n` days from the end of the month.
#'
#' @param wday `[integer / character]`
#'
#'    Days of the week to recur on. Integer values must be from `1` to `7`, with
#'    `1 = Monday` and `7 = Sunday`. This is also allowed to be a full weekday
#'    string like `"Tuesday"`, or an abbreviation like `"Tues"`.
#'
#' @param yweek `[integer]`
#'
#'    Weeks of the year to recur on. Integer values must be between
#'    `[1, 53]` or `[-53, -1]`.
#'
#' @keywords internal
#' @name deprecated-recur
NULL

#' @rdname deprecated-recur
#' @export
recur_on_mday <- function(x, mday) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_mday()",
    with = "recur_on_day_of_month()",
    always = TRUE
  )
  recur_on_day_of_month(x = x, day = mday)
}

#' @rdname deprecated-recur
#' @export
recur_on_wday <- function(x, wday, nth = NULL) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_wday()",
    with = "recur_on_day_of_week()",
    always = TRUE
  )
  recur_on_day_of_week(x = x, day = wday, nth = nth)
}

#' @rdname deprecated-recur
#' @export
recur_on_yday <- function(x, yday) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_yday()",
    with = "recur_on_day_of_year()",
    always = TRUE
  )
  recur_on_day_of_year(x = x, day = yday)
}

#' @rdname deprecated-recur
#' @export
recur_on_yweek <- function(x, yweek) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_yweek()",
    with = "recur_on_week_of_year()",
    always = TRUE
  )
  recur_on_week_of_year(x = x, week = yweek)
}

#' @rdname deprecated-recur
#' @export
recur_on_ymonth <- function(x, ymonth) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_ymonth()",
    with = "recur_on_month_of_year()",
    always = TRUE
  )
  recur_on_month_of_year(x = x, month = ymonth)
}

# ------------------------------------------------------------------------------

#' Deprecated rset helpers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' - `add_rschedule()` is deprecated in favor of using the `...` arguments of
#'   [runion()], [rintersect()], and [rsetdiff()] directly.
#'
#' - `add_rdates()` is deprecated in favor of using [runion()] in combination
#'   with a custom [rcustom()] rschedule holding the required dates.
#'
#' - `add_exdates()` is deprecated in favor of using [rsetdiff()] in combination
#'   with a custom [rcustom()] rschedule holding the excluded dates.
#'
#' @inheritParams adj_following
#'
#' @param x `[rset]`
#'
#'   An rset to add to.
#'
#' @param rdates `[Date]`
#'
#'   Dates to forcibly include in the rset.
#'
#' @param exdates `[Date]`
#'
#'   Dates to forcibly exclude from the rset.
#'
#' @return
#' An updated rset.
#'
#' @keywords internal
#' @name rset-add
#' @examples
#' on_thanksgiving <- yearly() %>%
#'   recur_on_day_of_week("Thurs", nth = 4) %>%
#'   recur_on_month_of_year("Nov")
#'
#' on_christmas <- yearly() %>%
#'   recur_on_day_of_month(25) %>%
#'   recur_on_month_of_year("Dec")
#'
#' on_labor_day <- monthly() %>%
#'   recur_on_month_of_year("Sep") %>%
#'   recur_on_day_of_week("Mon", nth = 1)
#'
#' # Rather than:
#' if (FALSE) {
#' rb <- runion() %>%
#'   add_rschedule(on_thanksgiving) %>%
#'   add_rschedule(on_christmas) %>%
#'   add_rschedule(on_labor_day)
#' }
#'
#' # Use the `...` of the `runion()` helper directly:
#' rb <- runion(on_thanksgiving, on_christmas, on_labor_day)
#'
#' # Thanksgiving, Christmas, or Labor Day
#' alma_search("2019-01-01", "2021-01-01", rb)
#'
#' # Except Labor Day in 2019
#' # Rather than:
#' if (FALSE) {
#' rb2 <- add_exdates(rb, "2019-09-02")
#' }
#'
#' # We recommend:
#' rb2 <- rsetdiff(rb, rcustom("2019-09-02"))
#'
#' alma_search("2019-01-01", "2021-01-01", rb2)
NULL

#' @rdname rset-add
#' @export
add_rschedule <- function(x, rschedule) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "add_rschedule()",
    with = I("the `...` argument of `runion()`, `rintersect()`, or `rsetdiff()`"),
    always = TRUE
  )

  check_rset(x)
  check_rschedule(rschedule)

  rschedules <- c(x$rschedules, list(rschedule))

  out <- new_rset(
    rschedules = rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )

  rset_restore(out, x)
}

#' @rdname rset-add
#' @export
add_rdates <- function(x, rdates) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "add_rdates()",
    with = I("`runion()` in combination with `rcustom()`"),
    always = TRUE
  )

  check_rset(x)
  rdates <- vec_cast_date(rdates)

  rdates <- vec_c(x$rdates, rdates)
  rdates <- vec_unique(rdates)
  rdates <- vec_sort(rdates)

  out <- new_rset(
    rschedules = x$rschedules,
    rdates = rdates,
    exdates = x$exdates
  )

  rset_restore(out, x)
}

#' @rdname rset-add
#' @export
add_exdates <- function(x, exdates) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "add_exdates()",
    with = I("`rsetdiff()` in combination with `rcustom()`"),
    always = TRUE
  )

  check_rset(x)
  exdates <- vec_cast_date(exdates)

  exdates <- vec_c(x$exdates, exdates)
  exdates <- vec_unique(exdates)
  exdates <- vec_sort(exdates)

  out <- new_rset(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = exdates
  )

  rset_restore(out, x)
}

check_rset <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  check_inherits(
    x = x,
    what = "almanac_rset",
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# Internal generic
rset_restore <- function(x, to) {
  UseMethod("rset_restore", to)
}

#' @export
rset_restore.almanac_rintersect <- function(x, to) {
  new_rintersect(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

#' @export
rset_restore.almanac_runion <- function(x, to) {
  new_runion(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

#' @export
rset_restore.almanac_rsetdiff <- function(x, to) {
  new_rsetdiff(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

# ------------------------------------------------------------------------------
