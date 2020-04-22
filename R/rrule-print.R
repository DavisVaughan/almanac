#' @export
print.rrule <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.rrule <- function(x, ...) {
  header <- format_header(x)
  body <- format_body(x)

  if (is.null(body)) {
    header
  } else {
    glue(header, body, .sep = "\n")
  }
}

# ------------------------------------------------------------------------------

format_body <- function(x) {
  x <- x$rules

  info <- c(
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

  if (is.null(info)) {
    return(NULL)
  }

  info <- paste0("- ", info)
  info <- glue::glue_collapse(info, sep = "\n")

  info
}

format_header <- function(x) {
  x <- x$rules

  info <- c(
    format_frequency(x),
    format_since(x),
    format_until(x)
  )

  info <- glue::glue_collapse(info, sep = " / ")

  glue("<rrule[{info}]>")
}

format_frequency <- function(x) {
  x$frequency
}

format_since <- function(x) {
  since <- x$since

  if (is.null(since)) {
    NULL
  } else {
    as.character(since)
  }
}

format_until <- function(x) {
  until <- x$until

  if (is.null(until)) {
    "???"
  } else {
    as.character(until)
  }
}

format_count <- function(x) {
  count <- x$count

  if (is.null(count)) {
    NULL
  } else {
    glue("count: {count}")
  }
}

format_interval <- function(x) {
  interval <- x$interval

  if (is.null(interval)) {
    NULL
  } else {
    glue("interval: {interval}")
  }
}

format_week_start <- function(x) {
  week_start <- x$week_start

  if (is.null(week_start)) {
    NULL
  } else {
    week_start <- weekday_abbr_print()[week_start]
    glue("week start: {week_start}")
  }
}

format_ymonth <- function(x) {
  ymonth <- x$ymonth

  if (is.null(ymonth)) {
    NULL
  } else {
    ymonth <- month.abb[ymonth]
    ymonth <- glue::glue_collapse(ymonth, sep = ", ")
    glue("ymonth: {ymonth}")
  }
}

format_yweek <- function(x) {
  yweek <- x$yweek

  if (is.null(yweek)) {
    NULL
  } else {
    yweek <- glue::glue_collapse(yweek, sep = ", ")
    glue("yweek: {yweek}")
  }
}

format_yday <- function(x) {
  yday <- x$yday

  if (is.null(yday)) {
    NULL
  } else {
    yday <- glue::glue_collapse(yday, sep = ", ")
    glue("yday: {yday}")
  }
}

format_mday <- function(x) {
  mday <- x$mday

  if (is.null(mday)) {
    NULL
  } else {
    mday <- glue::glue_collapse(mday, sep = ", ")
    glue("mday: {mday}")
  }
}

format_wday <- function(x) {
  wdays <- x$wday

  if (is.null(wdays)) {
    return(NULL)
  }

  out <- NULL

  for (i in seq_along(wdays)) {
    wday <- wdays[[i]]
    weekday <- weekday_abbr_print()[[i]]

    if (is.null(wday)) {
      next()
    }

    if (!identical(wday, "all")) {
      if (length(wday) > 5L) {
        wday <- c(wday[1:5], "...")
      }
      wday <- glue::glue_collapse(wday, sep = ", ")
      weekday <- glue("{weekday}[{wday}]")
    }

    out <- c(out, weekday)
  }

  out <- glue::glue_collapse(out, sep = ", ")

  glue("wday: {out}")
}

format_position <- function(x) {
  position <- x$position

  if (is.null(position)) {
    return(NULL)
  }

  if (length(position) > 5L) {
    position <- c(position[1:5], "...")
  }

  position <- glue::glue_collapse(position, sep = ", ")

  glue("position: {position}")
}

format_easter <- function(x) {
  easter <- x$easter

  if (is.null(easter)) {
    return(NULL)
  }

  glue("easter: offset = {easter}")
}
