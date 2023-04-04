
parse_js_date <- function(x) {
  if (length(x) == 0L) {
    return(new_date())
  }

  x <- lubridate::fast_strptime(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", lt = FALSE)

  as.Date(x)
}

# ------------------------------------------------------------------------------

as_js_from_date <- function(x) {
  milliseconds <- unclass(x) * 86400 * 1000
  glue("new Date({milliseconds})")
}

# ------------------------------------------------------------------------------

as_js_from_vector <- function(x) {
  x <- glue::glue_collapse(x, sep = ", ")
  glue("[{x}]")
}

# ------------------------------------------------------------------------------

as_js_from_boolean <- function(x) {
  if (x) {
    "true"
  } else {
    "false"
  }
}

# ------------------------------------------------------------------------------

as_js_from_rrule <- function(x) {
  rules <- c(
    get_dtstart(x),
    get_frequency(x),
    get_until(x),
    get_count(x),
    get_interval(x),
    get_week_start(x),
    get_ymonth(x),
    get_week_of_year(x),
    get_day_of_year(x),
    get_day_of_month(x),
    get_day_of_week(x),
    get_position(x),
    get_easter(x)
  )

  rules <- glue::glue_collapse(rules, sep = ",\n  ")

  js_rrule <- glue2(
    "new rrule.RRule({
      [[rules]]
    })")

  js_rrule
}

# ------------------------------------------------------------------------------

get_dtstart <- function(x) {
  dtstart <- x$since
  glue("dtstart: {as_js_from_date(dtstart)}")
}

get_frequency <- function(x) {
  frequency <- toupper(x$frequency)
  glue("freq: rrule.RRule.{frequency}")
}

get_until <- function(x) {
  until <- x$until

  # In case `recur_for_count()` is set
  if (is.null(until)) {
    return(NULL)
  }

  glue("until: {as_js_from_date(until)}")
}

get_count <- function(x) {
  if (is.null(x$count)) {
    return(NULL)
  }

  glue("count: {x$count}")
}

get_interval <- function(x) {
  if (is.null(x$interval)) {
    interval <- 1L
  } else {
    interval <- x$interval
  }

  glue("interval: {interval}")
}

get_week_start <- function(x) {
  if (is.null(x$week_start)) {
    week_start <- 0L # Monday, same as rrule.js
  } else {
    week_start <- x$week_start - 1L
  }

  glue("wkst: {week_start}")
}

get_day_of_month <- function(x) {
  if (is.null(x$day_of_month)) {
    return(NULL)
  }

  day_of_month <- as_js_from_vector(x$day_of_month)

  glue("bymonthday: {day_of_month}")
}

get_ymonth <- function(x) {
  if (is.null(x$ymonth)) {
    return(NULL)
  }

  ymonth <- as_js_from_vector(x$ymonth)

  glue("bymonth: {ymonth}")
}

get_week_of_year <- function(x) {
  if (is.null(x$week_of_year)) {
    return(NULL)
  }

  week_of_year <- as_js_from_vector(x$week_of_year)

  glue("byweekno: {week_of_year}")
}

get_day_of_year <- function(x) {
  if (is.null(x$day_of_year)) {
    return(NULL)
  }

  day_of_year <- as_js_from_vector(x$day_of_year)

  glue("byyearday: {day_of_year}")
}

get_day_of_week <- function(x) {
  if (is.null(x$day_of_week)) {
    return(NULL)
  }

  day_of_weeks <- x$day_of_week
  day_of_week_strings <- character()

  for (i in seq_along(day_of_weeks)) {
    day_of_week <- day_of_weeks[[i]]

    if (is.null(day_of_week)) {
      next
    }

    day_of_week_base <- get_js_day_of_week_base(i)

    if (identical(day_of_week, "all")) {
      day_of_week_strings <- c(day_of_week_strings, day_of_week_base)
      next
    }

    day_of_week_string <- glue("{day_of_week_base}.nth({day_of_week})")
    day_of_week_strings <- c(day_of_week_strings, day_of_week_string)
  }

  day_of_week_strings <- as_js_from_vector(day_of_week_strings)

  glue("byweekday: {day_of_week_strings}")
}

get_js_day_of_week_base <- function(day) {
  suffix <- switch(
    day,
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

get_position <- function(x) {
  if (is.null(x$position)) {
    return(NULL)
  }

  position <- as_js_from_vector(x$position)

  glue("bysetpos: {position}")
}

get_easter <- function(x) {
  if (is.null(x$easter)) {
    return(NULL)
  }

  glue("byeaster: {x$easter}")
}

# ------------------------------------------------------------------------------

append_rrule <- function(body, rules) {
  rules <- as_js_from_rrule(rules)

  glue("
    {body}

    ruleset.rrule(
      {rules}
    )
  ")
}

append_rdate <- function(body, rdate) {
  rdate <- as_js_from_date(rdate)

  glue("
    {body}

    ruleset.rdate(
      {rdate}
    )
  ")
}

append_exdate <- function(body, exdate) {
  exdate <- as_js_from_date(exdate)

  glue("
    {body}

    ruleset.exdate(
      {exdate}
    )
  ")
}

