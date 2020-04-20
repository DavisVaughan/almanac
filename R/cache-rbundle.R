Cache <- R6::R6Class(
  "Cache",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    set_rrules = function(rrules)
      cache__set_rrules(self, private, rrules),

    set_rdates = function(rdates)
      cache__set_rdates(self, private, rdates),

    set_exdates = function(exdates)
      cache__set_exdates(self, private, exdates),

    get_events = function()
      cache__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rrules = list(),
    rdates = new_date(),
    exdates = new_date(),

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache__cache_build <- function(self, private) {
  rrules <- private$rrules
  rdates <- private$rdates
  exdates <- private$exdates

  call <- cache_build_call(rrules, rdates, exdates)

  events <- almanac_global_context$call(call)
  events <- parse_js_date(events)

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

cache_build_call <- function(rrules, rdates, exdates) {
  body <- cache_build_call_body(rrules, rdates, exdates)
  as_js_build_call(body)
}

cache_build_call_body <- function(rrules, rdates, exdates) {
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

cache__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache__set_rrules <- function(self, private, rrules) {
  private$rrules <- rrules
  self
}

cache__set_rdates <- function(self, private, rdates) {
  private$rdates <- rdates
  self
}

cache__set_exdates <- function(self, private, exdates) {
  private$exdates <- exdates
  self
}

# ------------------------------------------------------------------------------

as_js_build_call <- function(body) {
  glue2("
    function() {
      [[body]]
      return ruleset.all()
    }
  ")
}

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

