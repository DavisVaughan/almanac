#' @rdname rbundle-set
#' @export
runion <- function(...) {
  rschedules <- list2(...)
  list_check_all_rschedules(rschedules)
  new_runion(rschedules = rschedules)
}

# ------------------------------------------------------------------------------

#' @rdname new-rbundle-set
#' @export
new_runion <- function(rschedules = list(),
                       rdates = new_date(),
                       exdates = new_date(),
                       ...,
                       class = character()) {
  cache <- cache_runion$new(
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
    class = c(class, "runion")
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.runion <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

#' @export
rbundle_restore.runion <- function(x, to) {
  new_runion(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

# ------------------------------------------------------------------------------

#' @export
print.runion <- function(x, ...) {
  rschedules <- x$rschedules
  n <- length(rschedules)

  cli::cli_text(cli::format_inline("<runion[{n}]>"))

  for (i in seq_len(n)) {
    cli_indented()
    print(rschedules[[i]])
    cli::cli_end()
  }

  invisible(x)
}
