cache <- R6::R6Class(
  "cache",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(min)
      cache__cache_initialize(self, private, min),

    cache_seq = function(recurrences, to)
      cache__cache_seq(self, recurrences, to),

    cache_next = function(recurrences, current, inclusive)
      cache__cache_next(self, recurrences, current, inclusive),

    cache_previous = function(recurrences, current, inclusive)
      cache__cache_previous(self, recurrences, current, inclusive),

    slice_seq = function(from, to, inclusive)
      cache__slice_seq(self, from, to, inclusive),

    slice_next = function(current, inclusive)
      cache__slice_next(self, current, inclusive),

    slice_previous = function(current, inclusive)
      cache__slice_previous(self, current, inclusive),

    get_data = function()
      cache__get_data(self, private),

    get_min = function()
      cache__get_min(self, private),

    get_max = function()
      cache__get_max(self, private),

    is_empty = function()
      cache__is_empty(self, private),

    is_complete = function()
      cache__is_complete(self, private),

    set_complete = function()
      cache__set_complete(self, private),

    set_data = function(data)
      cache__set_data(self, private, data),

    set_max = function(max)
      cache__set_max(self, private, max)

  ),

  # ----------------------------------------------------------------------------
  private = list(

    min = NULL,
    max = NULL,

    data = NULL,

    complete = FALSE

  )
)

# ------------------------------------------------------------------------------

cache__cache_seq <- function(self, recurrences, to) {
  from <- self$get_min()

  # Nothing to cache, below the first possible occurrence
  if (to < from) {
    self$set_data(almanac_global_empty_date)
    self$set_max(to)
    return(self)
  }

  # Negative inf should be covered by the above condition,
  # but positive infinity can happen
  if (identical(to, almanac_global_inf_date)) {
    stop_cache_infinite_extension()
  }

  call <- cache_events_call(recurrences)

  arg_from <- as_js_from_date(from)
  arg_from <- V8::JS(arg_from)

  arg_to <- as_js_from_date(to)
  arg_to <- V8::JS(arg_to)

  events <- almanac_global_context$call(call, arg_from, arg_to)
  events <- parse_js_date(events)

  self$set_data(events)
  self$set_max(to)

  self
}

cache_events_call <- function(recurrences) {
  body <- rset_setup_call_body(recurrences)

  glue2("
    function(from, to) {
      [[body]]

      return ruleset.between(from, to, inc = true)
    }
  ")
}

# ------------------------------------------------------------------------------

cache__slice_seq <- function(self, from, to, inclusive) {
  if (self$is_empty()) {
    return(NULL)
  }

  max <- self$get_max()

  # After end of cache
  if (max < to) {
    return(NULL)
  }

  # Can happen with empty / all NA input where `max2()` returns `-Inf`
  # and `min2()` returns `Inf`
  if (from > to) {
    return(almanac_global_empty_date)
  }

  data <- self$get_data()

  # TODO: Optimize with C, just get the start:end ranges
  # since `data` is sorted
  if (inclusive) {
    locs <- data >= from & data <= to
  } else {
    locs <- data > from & data < to
  }

  data <- data[locs]

  data
}

# ------------------------------------------------------------------------------

cache__cache_next <- function(self, recurrences, current, inclusive) {
  call <- cache_next_call(recurrences)

  from <- self$get_min()
  arg_from <- as_js_from_date(from)
  arg_from <- V8::JS(arg_from)

  arg_current <- as_js_from_date(current)
  arg_current <- V8::JS(arg_current)

  arg_inclusive <- as_js_from_boolean(inclusive)
  arg_inclusive <- V8::JS(arg_inclusive)

  events <- almanac_global_context$call(call, arg_current, arg_inclusive, arg_from)

  if (is_complete_signal(events)) {
    self$set_complete()
    self$set_max(current)

    # It it was an empty cache before, we now know it had zero occurrences
    if (self$is_empty()) {
      self$set_data(almanac_global_empty_date)
    }

    return(self)
  }

  events <- parse_js_date(events)

  max <- events[[length(events)]]

  self$set_data(events)
  self$set_max(max)

  self
}

# If `after` is `null`, there are no recurrences
# left after the current one, and the cache is complete
cache_next_call <- function(recurrences) {
  body <- rset_setup_call_body(recurrences)

  glue2("
    function(current, inclusive, from) {
      [[body]]

      var after = ruleset.after(current, inc = inclusive)

      if (after == null) {
        return 'complete'
      }

      return ruleset.between(from, after, inc = true)
    }
  ")
}

# ------------------------------------------------------------------------------

cache__slice_next <- function(self, current, inclusive) {
  if (self$is_empty()) {
    return(NULL)
  }

  data <- self$get_data()

  max <- self$get_max()

  # After maximum checked date in the cache
  # There could still be events at a date past the max checked one
  if (max < current) {
    return(NULL)
  }

  # Attempt to locate the first cached event after `current`
  if (inclusive) {
    indicator <- current <= data
  } else {
    indicator <- current < data
  }

  if (!any(indicator)) {
    if (self$is_complete()) {
      # No events after `current`
      return(almanac_global_na_date)
    } else {
      # `current` might be before the maximum checked date, but after
      # the last known occurrence, in which case we need to try and
      # extend the cache with `cache__cache_next()`
      return(NULL)
    }
  }

  loc <- which(indicator)
  loc <- loc[[1]]

  data[[loc]]
}

# ------------------------------------------------------------------------------

cache__cache_previous <- function(self, recurrences, current, inclusive) {
  # Cache up to the `current` date, `cache_seq()` will always be inclusive
  self$cache_seq(recurrences, current)
}

# ------------------------------------------------------------------------------

cache__slice_previous <- function(self, current, inclusive) {
  if (self$is_empty()) {
    return(NULL)
  }

  data <- self$get_data()
  min <- self$get_min()

  # If before the min, definitely no previous event possible
  # returns `NA` because we guarantee a size 1 return value
  if (inclusive) {
    if (current < min) {
      return(almanac_global_na_date)
    }
  } else {
    if (current <= min) {
      return(almanac_global_na_date)
    }
  }

  # If `current` is past the max, we need to get all events up to it
  # to determine the previous event
  max <- self$get_max()

  # After end of cache
  if (max < current) {
    return(NULL)
  }

  # Attempt to locate the first cached event before `current`
  if (inclusive) {
    indicator <- current >= data
  } else {
    indicator <- current > data
  }

  # There are no events before `current`
  if (!any(indicator)) {
    return(almanac_global_na_date)
  }

  # Take the last one, the one closest to `current`
  loc <- which(indicator)
  loc <- loc[[length(loc)]]

  data[[loc]]
}

# ------------------------------------------------------------------------------

cache__get_data <- function(self, private) {
  private$data
}

cache__get_min <- function(self, private) {
  private$min
}

cache__get_max <- function(self, private) {
  private$max
}

cache__set_data <- function(self, private, data) {
  private$data <- data
  self
}

cache__set_max <- function(self, private, max) {
  private$max <- max
  self
}

# ------------------------------------------------------------------------------

cache__cache_initialize <- function(self, private, min) {
  private$min <- min
  self
}

# ------------------------------------------------------------------------------

rset_setup_call_body <- function(recurrences) {
  rrules <- recurrences$rrules
  rdates <- recurrences$rdates
  exdates <- recurrences$exdates

  body <- "var ruleset = new rrule.RRuleSet()"

  for(rrule in rrules) {
    body <- append_rrule(body, rrule)
  }

  for(i in seq_along(rdates)) {
    rdate <- rdates[i]
    body <- append_rdate(body, rdate)
  }

  for(i in seq_along(exdates)) {
    exdate <- exdates[i]
    body <- append_exdate(body, exdate)
  }

  body
}

append_rrule <- function(body, rrule) {
  rrule <- as_js_from_rrule(rrule)

  glue("
    {body}

    ruleset.rrule(
      {rrule}
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

# ------------------------------------------------------------------------------

cache__is_empty <- function(self, private) {
  is.null(private$data)
}

cache__is_complete <- function(self, private) {
  private$complete
}

cache__set_complete <- function(self, private) {
  private$complete <- TRUE
  self
}

is_complete_signal <- function(x) {
  identical(x, "complete")
}

# ------------------------------------------------------------------------------

is_empty_recurrence_set <- function(recurrences) {
  length(recurrences$rrules) == 0L &&
    length(recurrences$rdates) == 0L
}
