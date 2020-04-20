Cache <- R6::R6Class(
  "Cache",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    set_recurrences = function(recurrences)
      cache__set_recurrences(self, private, recurrences),

    get = function()
      cache__get(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    recurrences = NULL,
    events = NULL,
    built = FALSE,

    cache_build = function()
      cache__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache__cache_build <- function(self, private) {
  recurrences <- private$recurrences

  call <- cache_build_call(recurrences)

  events <- almanac_global_context$call(call)
  events <- parse_js_date(events)

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

cache_build_call <- function(recurrences) {
  body <- as_js_call_body(recurrences)

  glue2("
    function() {
      [[body]]
      return ruleset.all()
    }
  ")
}

# ------------------------------------------------------------------------------

cache__get <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache__set_recurrences <- function(self, private, recurrences) {
  private$recurrences <- recurrences
  self
}

# ------------------------------------------------------------------------------

as_js_call_body <- function(recurrences) {
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

parse_js_date <- function(x) {
  if (length(x) == 0L) {
    return(new_date())
  }

  x <- fast_strptime(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", lt = FALSE)

  as.Date(x)
}

