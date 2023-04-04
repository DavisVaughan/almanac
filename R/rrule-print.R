#' @export
print.rrule <- function(x, ...) {
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
    format_ymonth(x),
    format_yweek(x),
    format_yday(x),
    format_mday(x),
    format_wday(x),
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
    week_start <- weekday_abbr_print()[week_start]
    cli::format_inline("week start: {week_start}")
  }
}

format_ymonth <- function(x) {
  ymonth <- x$ymonth

  if (is.null(ymonth)) {
    character()
  } else {
    ymonth <- month.abb[ymonth]
    cli::format_inline("month of year: {ymonth}")
  }
}

format_yweek <- function(x) {
  yweek <- x$yweek

  if (is.null(yweek)) {
    character()
  } else {
    cli::format_inline("week of year: {yweek}")
  }
}

format_yday <- function(x) {
  yday <- x$yday

  if (is.null(yday)) {
    character()
  } else {
    cli::format_inline("day of year: {yday}")
  }
}

format_mday <- function(x) {
  mday <- x$mday

  if (is.null(mday)) {
    character()
  } else {
    cli::format_inline("day of month: {mday}")
  }
}

format_wday <- function(x) {
  wdays <- x$wday

  if (is.null(wdays)) {
    return(character())
  }

  out <- character()

  for (i in seq_along(wdays)) {
    wday <- wdays[[i]]
    weekday <- weekday_abbr_print()[[i]]

    if (is.null(wday)) {
      next()
    }

    if (!identical(wday, "all")) {
      wday <- cli::ansi_collapse(wday, sep = ", ", last = ", ")
      weekday <- cli::format_inline("{weekday}[{wday}]")
    }

    out <- c(out, weekday)
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

  cli::format_inline("easter: offset = {easter}")
}
