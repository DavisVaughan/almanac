as_js_from_date <- function(x) {
  x <- as.POSIXlt(x)
  glue("new Date(Date.UTC({year(x)}, {month(x) - 1L}, {day(x)}))")
}

# ------------------------------------------------------------------------------

as_js_from_rrule <- function(x, context) {
  rules <- c(
    get_dtstart(x),
    get_frequency(x),
    get_until(x),
    get_times(x),
    get_interval(x),
    get_week_start(x),
    get_ymonth(x, context),
    get_yweek(x, context),
    get_yday(x, context),
    get_mday(x, context),
    get_wday(x),
    get_position(x, context),
    get_easter(x)
  )

  rules <- glue::glue_collapse(rules, sep = ",\n  ")

  js_rrule <- glue2(
    "new rrule.RRule({
      [[rules]]
    })")

  js_rrule
}

get_dtstart <- function(x) {
  glue("dtstart: {as_js_from_date(x$rules$since)}")
}

get_frequency <- function(x) {
  frequency <- toupper(x$rules$frequency)
  glue("freq: rrule.RRule.{frequency}")
}

get_until <- function(x) {
  if (is.null(x$rules$until)) {
    return(NULL)
  }

  glue("until: {as_js_from_date(x$rules$until)}")
}

get_times <- function(x) {
  if (is.null(x$rules$times)) {
    return(NULL)
  }

  glue("count: {x$rules$times}")
}

get_interval <- function(x) {
  if (is.null(x$rules$interval)) {
    interval <- 1L
  } else {
    interval <- x$rules$interval
  }

  glue("interval: {interval}")
}

get_week_start <- function(x) {
  if (is.null(x$rules$week_start)) {
    week_start <- 0L # Monday, same as rrule.js
  } else {
    week_start <- x$rules$week_start - 1L
  }

  glue("wkst: {week_start}")
}

get_mday <- function(x, context) {
  if (is.null(x$rules$mday)) {
    return(NULL)
  }

  v8_assign(context, "mday", x$rules$mday)

  glue("bymonthday: mday")
}

get_ymonth <- function(x, context) {
  if (is.null(x$rules$ymonth)) {
    return(NULL)
  }

  v8_assign(context, "ymonth", x$rules$ymonth)

  glue("bymonth: ymonth")
}

get_yweek <- function(x, context) {
  if (is.null(x$rules$yweek)) {
    return(NULL)
  }

  v8_assign(context, "yweek", x$rules$yweek)

  glue("byweekno: yweek")
}

get_yday <- function(x, context) {
  if (is.null(x$rules$yday)) {
    return(NULL)
  }

  v8_assign(context, "yday", x$rules$yday)

  glue("byyearday: yday")
}

get_wday <- function(x) {
  if (is.null(x$rules$wday)) {
    return(NULL)
  }

  wdays <- x$rules$wday
  wday_strings <- character()

  for (i in seq_along(wdays)) {
    wday <- wdays[[i]]

    if (is.null(wday)) {
      next
    }

    wday_base <- get_js_wday_base(i)

    if (identical(wday, "all")) {
      wday_strings <- c(wday_strings, wday_base)
      next
    }

    wday <- glue::glue_collapse(wday, sep = ", ")
    wday_nth <- glue("{wday_base}.nth({wday})")

    wday_strings <- c(wday_strings, wday_nth)
  }

  wday_strings <- glue::glue_collapse(wday_strings, sep = ", ")

  glue("byweekday: [{wday_strings}]")
}

get_js_wday_base <- function(wday) {
  suffix <- switch(
    wday,
    `1` = "MO",
    `2` = "TU",
    `3` = "WE",
    `4` = "TH",
    `5` = "FR",
    `6` = "SA",
    `7` = "SU"
  )

  glue("rrule.RRule.{suffix}")
}

get_position <- function(x, context) {
  if (is.null(x$rules$position)) {
    return(NULL)
  }

  v8_assign(context, "position", x$rules$position)

  glue("bysetpos: position")
}

get_easter <- function(x) {
  if (is.null(x$rules$easter)) {
    return(NULL)
  }

  glue("byeaster: {x$rules$easter}")
}
