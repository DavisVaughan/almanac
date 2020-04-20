#' Recur on easter
#'
#' `recur_on_easter()` is a special helper to recur on Easter. Easter is
#' particularly difficult to construct a recurrence rule for. Using `offset`,
#' this can also be used to generate a recurrence rule on Easter Monday or
#' Good Friday.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param offset `[integer(1)]`
#'
#'    An offset in terms of a number of days on either side of Easter to recur
#'    on. This offset must still fall within the same year, otherwise the date
#'    will be silently ignored.
#'
#' @examples
#' on_easter <- yearly() %>% recur_on_easter()
#' on_easter_monday <- yearly() %>% recur_on_easter(-1)
#'
#' alma_search("1999-01-01", "2001-01-01", on_easter)
#'
#' sch <- schedule() %>%
#'   sch_rrule(on_easter) %>%
#'   sch_rrule(on_easter_monday)
#'
#' alma_search("1999-01-01", "2001-01-01", sch)
#'
#'
#' # Note that `offset` must land within the same year, otherwise the date
#' # is ignored
#' on_easter_back_93_days <- yearly() %>% recur_on_easter(-93)
#' on_easter_back_94_days <- yearly() %>% recur_on_easter(-94)
#'
#' alma_search("1999-01-01", "2001-01-01", on_easter_back_93_days)
#' alma_search("1999-01-01", "2001-01-01", on_easter_back_94_days)
#'
#' @noRd
NULL

# `recur_on_easter()` is exposed through `hldy_on_easter()`

recur_on_easter <- function(x, offset = 0L) {
  validate_rrule(x)

  if (is_already_set(x, "easter")) {
    abort("The `easter` rule has already been set.")
  }

  offset <- vec_cast(offset, integer(), x_arg = "offset")
  vec_assert(offset, size = 1L, arg = "offset")

  abs_offset <- abs(offset)
  if (abs_offset > 366) {
    abort("`offset` can only take values in [-366, 366].")
  }

  tweak_rrule(x, easter = offset)
}
