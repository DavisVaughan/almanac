new_rcalendar <- function(rholidays = list(),
                          ...,
                          class = character()) {
  vec_check_list(rholidays)

  cache <- cache_rcalendar$new(
    rholidays = rholidays
  )

  new_rschedule(
    rholidays = rholidays,
    cache = cache,
    ...,
    class = c(class, "rcalendar")
  )
}

rcalendar <- function(...) {
  rholidays <- list2(...)
  new_rcalendar(rholidays = rholidays)
}

#' @export
print.rcalendar <- function(x, ...) {
  cat_line(format(x))
  invisible(x)
}

#' @export
format.rcalendar <- function(x, ...) {
  names <- cal_names(x)
  n_names <- length(names)

  header <- glue("<rcalendar[{n_names}]>")

  bullets <- vec_paste0("- ", names)

  out <- glue::glue_collapse(c(header, bullets), sep = "\n")
  out <- as.character(out)

  out
}

#' @export
rschedule_events.rcalendar <- function(x) {
  x$cache$get_events()
}

cal_events <- function(x, ..., year = NULL) {
  check_dots_empty0(...)
  check_rcalendar(x)

  out <- x$cache$get_events_frame()

  if (!is_null(year)) {
    years <- date_year(out$date)
    out <- vec_slice(out, years %in% year)
  }

  out
}

cal_holidays <- function(x) {
  check_rcalendar(x)
  out <- x$rholidays
  names(out) <- cal_names(x)
  out
}

cal_names <- function(x) {
  check_rcalendar(x)
  x$cache$get_names()
}

cal_add <- function(x, y) {
  check_rcalendar(x)
  # check_rholiday(y)
  name <- y$name
  names <- cal_names(x)

  if (name %in% names) {
    # TODO: Improve this
    abort("Can't add duplicate holidays.")
  }

  rholidays <- c(x$rholidays, list(y))

  new_rcalendar(rholidays = rholidays)
}

cal_remove <- function(x, name) {
  check_rcalendar(x)
  check_character(name)

  names <- cal_names(x)

  if (!all(name %in% names)) {
    abort("Can't remove names that don't exist.")
  }

  new <- vec_set_difference(names, name)
  loc <- vec_match(new, names)

  rholidays <- x$rholidays
  rholidays <- rholidays[loc]

  new_rcalendar(rholidays = rholidays)
}

cal_match <- function(needles, haystack) {
  # check_date(needles, allow_na = FALSE, allow_infinite = FALSE)
  needles <- vec_cast_date(needles, "needles")
  check_rcalendar(haystack)

  frame <- cal_events(haystack)

  loc <- vec_match(needles, frame$date)
  out <- vec_slice(frame$name, loc)

  out
}

cal_federal_united_states <- function() {
  on_weekends <- adjust_on_weekends()

  rcalendar(
    hol_adjust(hol_new_years_day(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_martin_luther_king_junior_day(),
    hol_us_presidents_day(),
    hol_us_memorial_day(),
    hol_adjust(hol_us_juneteenth(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_adjust(hol_us_independence_day(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_labor_day(),
    hol_us_indigenous_peoples_day(),
    hol_adjust(hol_us_veterans_day(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_thanksgiving(),
    hol_adjust(hol_christmas(), adjust_on = on_weekends, adjustment = adj_nearest)
  )
}

cal_posit <- function() {
  on_weekends <- adjust_on_weekends()

  hol_massachusetts_patriots_day <- function() {
    out <- yearly()
    out <- recur_on_ymonth(out, "April")
    out <- recur_on_wday(out, "Monday", nth = 3L)

    new_rholiday(
      name = "Massachusetts Patriot's Day",
      rschedule = out
    )
  }

  # - Adjust Christmas to nearest workday
  # - Then offset that forward one day
  # - Then adjust that to following workday (always adjust forward in case
  #   Christmas was on a Friday, in which case the day after is Saturday, but
  #   observed on Monday)
  on_day_after_christmas <- hol_christmas() %>%
    hol_adjust(adjust_on = on_weekends, adjustment = adj_nearest) %>%
    hol_offset(offset = 1L) %>%
    hol_adjust(adjust_on = on_weekends, adjustment = adj_following) %>%
    hol_rename("Day after Christmas")

  rcalendar(
    hol_adjust(hol_new_years_day(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_martin_luther_king_junior_day(),
    hol_us_presidents_day(),
    hol_massachusetts_patriots_day(),
    hol_us_memorial_day(),
    hol_adjust(hol_us_juneteenth(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_adjust(hol_us_independence_day(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_labor_day(),
    hol_us_indigenous_peoples_day(),
    hol_us_election_day(),
    hol_adjust(hol_us_veterans_day(), adjust_on = on_weekends, adjustment = adj_nearest),
    hol_us_thanksgiving(),
    hol_adjust(hol_christmas(), adjust_on = on_weekends, adjustment = adj_nearest),
    on_day_after_christmas
  )
}

is_rcalendar <- function(x) {
  inherits(x, "rcalendar")
}

check_rcalendar <- function(x,
                            ...,
                            allow_null = FALSE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  if (!missing(x)) {
    if (is_rcalendar(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an <rcalendar>",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

