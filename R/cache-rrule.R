CacheRrule <- R6::R6Class(
  "CacheRrule",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rules)
      cacherrule__initialize(self, private, rules),

    get_events = function()
      cacherrule__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rules = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cacherrule__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cacherrule__cache_build <- function(self, private) {
  rules <- private$rules

  call <- cacherrule_build_call(rules)

  events <- almanac_global_context$call(call)
  events <- parse_js_date(events)

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

cacherrule_build_call <- function(rules) {
  body <- cacherrule_build_call_body(rules)
  as_js_build_call(body)
}

cacherrule_build_call_body <- function(rules) {
  body <- "var ruleset = new rrule.RRuleSet()"
  body <- append_rrule(body, rules)
  body
}

# ------------------------------------------------------------------------------

cacherrule__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cacherrule__initialize <- function(self, private, rules) {
  private$rules <- rules
  self
}

