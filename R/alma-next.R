#' Generate the next or previous event
#'
#' @description
#'
#' - `alma_next()` generates the next event after `x`.
#'
#' - `alma_previous()` generates the previous event before `x`.
#'
#' @inheritParams alma_seq
#'
#' @param x `[Date(1)]`
#'
#'    The date to start the search from.
#'
#' @examples
#' on_12th <- monthly() %>% recur_on_mday(12)
#' on_monday <- weekly() %>% recur_on_wday("Monday")
#'
#' # On the 12th of the month, or on Mondays
#' sch <- schedule() %>%
#'   sch_rrule(on_12th) %>%
#'   sch_rrule(on_monday)
#'
#' alma_next("2019-01-01", sch)
#'
#' alma_previous("2019-01-01", sch)
#'
#' @export
alma_next <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  schedule <- as_schedule(schedule)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  alma_next_impl(x, schedule, inclusive)
}

alma_next_impl <- function(x, schedule, inclusive) {
  occurrences <- schedule$cache$get()

  out <- vector("numeric", length(x))

  # Avoid dispatch overhead
  x <- unclass(x)
  occurrences <- unclass(occurrences)

  # TODO: Reimplement in C with a comparison loop
  # or C++ with std::lower_bound? It would give the exact value,
  # and if itr==end then we use NA_REAL.
  for(i in seq_along(x)) {
    elt <- x[[i]]

    if (inclusive) {
      where <- elt <= occurrences
    } else {
      where <- elt < occurrences
    }

    if (!any(where)) {
      out[[i]] <- NA_real_
      next
    }

    locs <- which(where)
    loc <- locs[[1L]]
    out[[i]] <- occurrences[[loc]]
  }

  out <- new_date(out)

  out
}

#' @rdname alma_next
#' @export
alma_previous <- function(x, schedule, inclusive = FALSE) {
  x <- vec_cast_date(x)

  schedule <- as_schedule(schedule)

  vec_assert(inclusive, logical(), 1L)
  if (is.na(inclusive)) {
    abort("`inclusive` cannot be `NA`")
  }

  alma_previous_impl(x, schedule, inclusive)
}

alma_previous_impl <- function(x, schedule, inclusive) {
  occurrences <- schedule$cache$get()

  out <- vector("numeric", length(x))

  # Avoid dispatch overhead
  x <- unclass(x)
  occurrences <- unclass(occurrences)

  # TODO: Reimplement in C with std::upper_bound - 1?
  # https://stackoverflow.com/questions/9989731/algorithm-function-for-finding-last-item-less-than-or-equal-to-like-lower-bou
  for(i in seq_along(x)) {
    elt <- x[[i]]

    if (inclusive) {
      where <- elt >= occurrences
    } else {
      where <- elt > occurrences
    }

    if (!any(where)) {
      out[[i]] <- NA_real_
      next
    }

    locs <- which(where)
    loc <- locs[[length(locs)]]
    out[[i]] <- occurrences[[loc]]
  }

  out <- new_date(out)

  out
}
