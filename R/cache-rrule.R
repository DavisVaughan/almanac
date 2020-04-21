cache_rrule <- R6::R6Class(
  "cache_rrule",
  cloneable = FALSE,

  # ----------------------------------------------------------------------------
  public = list(
    initialize = function(rules)
      cache_rrule__initialize(self, private, rules),

    get_events = function()
      cache_rrule__get_events(self, private)
  ),

  # ----------------------------------------------------------------------------
  private = list(
    rules = NULL,

    events = NULL,
    built = FALSE,

    cache_build = function()
      cache_rrule__cache_build(self, private)
  )
)

# ------------------------------------------------------------------------------

cache_rrule__cache_build <- function(self, private) {
  rules <- private$rules

  call <- cache_rrule_build_call(rules)

  events <- almanac_global_context$call(call)
  events <- parse_js_date(events)

  private$events <- events
  private$built <- TRUE

  invisible(self)
}

cache_rrule_build_call <- function(rules) {
  body <- cache_rrule_build_call_body(rules)

  glue2("
    function() {
      [[body]]
      return rule.all()
    }
  ")
}

cache_rrule_build_call_body <- function(rules) {
  rrule <- as_js_from_rrule(rules)
  body <- glue("var rule = {rrule}")
  body
}

# ------------------------------------------------------------------------------

cache_rrule__get_events <- function(self, private) {
  if (!private$built) {
    private$cache_build()
  }

  private$events
}

# ------------------------------------------------------------------------------

cache_rrule__initialize <- function(self, private, rules) {
  private$rules <- rules
  self
}

