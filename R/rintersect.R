#' @rdname rbundle-set
#' @export
rintersect <- function(...) {
  rschedules <- list2(...)
  list_check_all_rschedules(rschedules)
  new_rintersect(rschedules = rschedules)
}

new_rintersect <- function(rschedules = list(),
                           rdates = new_date(),
                           exdates = new_date(),
                           ...,
                           class = character()) {
  cache <- cache_rintersect$new(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates
  )

  new_rbundle(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    cache = cache,
    ...,
    class = c(class, "rintersect")
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.rintersect <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

#' @export
print.rintersect <- function(x, ...) {
  rschedules <- x$rschedules
  n <- length(rschedules)

  cli::cli_text(cli::format_inline("<rintersect[{n}]>"))

  for (i in seq_len(n)) {
    cli_indented()
    print(rschedules[[i]])
    cli::cli_end()
  }

  invisible(x)
}
