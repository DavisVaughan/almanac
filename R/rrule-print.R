#' @export
print.almanac_rrule <- function(x, ...) {
  cli::cli_text("<rrule>")
  cli::cli_ul(format_body(x))
  invisible(x)
}

# ------------------------------------------------------------------------------

format_body <- function(x) {
  x <- x$rules

  info <- c(
    format_frequency(x),
    format_since(x),
    format_until(x),
    format_count(x),
    format_interval(x),
    format_week_start(x),
    format_month_of_year(x),
    format_week_of_year(x),
    format_day_of_year(x),
    format_day_of_month(x),
    format_day_of_week(x),
    format_position(x),
    format_easter(x)
  )

  set_names(info, "*")
}

format_frequency <- function(x) {
  cli::format_inline("frequency: {x$frequency}")
}

format_since <- function(x) {
  cli::format_inline("since: {x$since}")
}

format_until <- function(x) {
  until <- x$until

  if (is.null(until)) {
    # Can be overriden when setting `count`
    character()
  } else {
    cli::format_inline("until: {until}")
  }
}

format_count <- function(x) {
  count <- x$count

  if (is.null(count)) {
    character()
  } else {
    cli::format_inline("count: {count}")
  }
}

format_interval <- function(x) {
  interval <- x$interval

  if (is.null(interval)) {
    character()
  } else {
    cli::format_inline("interval: {interval}")
  }
}

format_week_start <- function(x) {
  week_start <- x$week_start

  if (is.null(week_start)) {
    character()
  } else {
    week_start <- day_of_week_abbr()[week_start]
    cli::format_inline("week start: {week_start}")
  }
}

format_month_of_year <- function(x) {
  month_of_year <- x$month_of_year

  if (is.null(month_of_year)) {
    character()
  } else {
    month_of_year <- month.abb[month_of_year]
    cli::format_inline("month of year: {month_of_year}")
  }
}

format_week_of_year <- function(x) {
  week_of_year <- x$week_of_year

  if (is.null(week_of_year)) {
    character()
  } else {
    cli::format_inline("week of year: {week_of_year}")
  }
}

format_day_of_year <- function(x) {
  day_of_year <- x$day_of_year

  if (is.null(day_of_year)) {
    character()
  } else {
    cli::format_inline("day of year: {day_of_year}")
  }
}

format_day_of_month <- function(x) {
  day_of_month <- x$day_of_month

  if (is.null(day_of_month)) {
    character()
  } else {
    cli::format_inline("day of month: {day_of_month}")
  }
}

format_day_of_week <- function(x) {
  day_of_weeks <- x$day_of_week

  if (is.null(day_of_weeks)) {
    return(character())
  }

  out <- character()

  for (i in seq_along(day_of_weeks)) {
    day_of_week <- day_of_weeks[[i]]
    day_of_week_string <- day_of_week_abbr()[[i]]

    if (is.null(day_of_week)) {
      next()
    }

    if (!identical(day_of_week, "all")) {
      day_of_week <- cli::ansi_collapse(day_of_week, sep = ", ", last = ", ")
      day_of_week_string <- cli::format_inline("{day_of_week_string}[{day_of_week}]")
    }

    out <- c(out, day_of_week_string)
  }

  out <- cli::ansi_collapse(out)

  cli::format_inline("day of week: {out}")
}

format_position <- function(x) {
  position <- x$position

  if (is.null(position)) {
    return(character())
  }

  cli::format_inline("position: {position}")
}

format_easter <- function(x) {
  easter <- x$easter

  if (is.null(easter)) {
    return(character())
  }

  if (identical(easter, 0L)) {
    cli::format_inline("easter")
  } else {
    cli::format_inline("easter: offset = {easter}")
  }
}
