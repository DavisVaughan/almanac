as_js_rrule <- function(x) {
  rules <- c(
    get_dtstart(x),
    get_frequency(x),
    get_until(x),
    get_times(x),
    get_interval(x),
    get_week_start(x),
    get_yweek(x),
    get_mday(x)
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
    week_start <- 1L # Monday, same as rrule.js
  } else {
    week_start <- x$rules$week_start
  }

  glue("wkst: {week_start}")
}

get_mday <- function(x) {
  if (is.null(x$rules$mday)) {
    return(NULL)
  }

  context <- get_context(x)

  v8_assign(context, "mday", x$rules$mday)

  glue("bymonthday: mday")
}

get_yweek <- function(x) {
  if (is.null(x$rules$yweek)) {
    return(NULL)
  }

  context <- get_context(x)

  v8_assign(context, "yweek", x$rules$yweek)

  glue("byweekno: yweek")
}
