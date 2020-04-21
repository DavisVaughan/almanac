cache_rbundle <- R6::R6Class(
  "cache_rbundle",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(cachers, rdates, exdates)
      cache_rbundle__initialize(self, private, cachers, rdates, exdates),

    get_events = function()
      cache_rbundle__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    cachers = list(),
    rdates = new_date(),
    exdates = new_date(),

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_rbundle__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_rbundle__cache_build <- function(self, private) {
  if (all_rrules(private$cachers)) {
    # When all `cachers` are `rrules`, we can optimize into 1 JS call
    cache_rbundle__cache_build_rrules(self, private)
  } else {
    cache_rbundle__cache_build_impl(self, private)
  }
}

cache_rbundle__cache_build_impl <- function(self, private) {
  cachers <- private$cachers
  rdates <- private$rdates
  exdates <- private$exdates

  # Get events for each cacher
  cachers_events <- map(cachers, cacher_events)

  # Forcibly include `rdates`
  if (!vec_is_empty(rdates)) {
    cachers_events <- c(cachers_events, list(rdates))
  }

  # Combine, sort, and uniquify
  events <- vec_unchop(cachers_events, ptype = new_date())
  events <- vec_unique(events)
  events <- vec_sort(events)

  # Forcibly remove `exdates`
  if (!vec_is_empty(exdates)) {
    events <- vec_set_diff(events, exdates)
  }

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

cache_rbundle__cache_build_rrules <- function(self, private) {
  rrules <- private$cachers
  rdates <- private$rdates
  exdates <- private$exdates

  call <- cache_rbundle_build_call(rrules, rdates, exdates)

  events <- almanac_global_context$call(call)
  events <- parse_js_date(events)

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

cache_rbundle_build_call <- function(rrules, rdates, exdates) {
  body <- cache_rbundle_build_call_body(rrules, rdates, exdates)

  glue2("
    function() {
      [[body]]
      return ruleset.all()
    }
  ")
}

cache_rbundle_build_call_body <- function(rrules, rdates, exdates) {
  body <- "var ruleset = new rrule.RRuleSet()"

  for(rrule in rrules) {
    rules <- rrule$rules
    body <- append_rrule(body, rules)
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

# ------------------------------------------------------------------------------

cache_rbundle__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache_rbundle__initialize <- function(self, private, cachers, rdates, exdates) {
  private$cachers <- cachers
  private$rdates <- rdates
  private$exdates <- exdates
  self
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

# ------------------------------------------------------------------------------

parse_js_date <- function(x) {
  if (length(x) == 0L) {
    return(new_date())
  }

  x <- lubridate::fast_strptime(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", lt = FALSE)

  as.Date(x)
}

all_rrules <- function(x) {
  all(map_lgl(x, is_rrule))
}
