new_rsubclass <- function(rschedules = list(),
                          rdates = new_date(),
                          exdates = new_date(),
                          foo = numeric()) {
  new_rbundle(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    foo = foo,
    class = "rsubclass"
  )
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}

local_rsubclass <- function(frame = caller_env()) {
  local_methods(
    .frame = frame,
    rbundle_restore.rsubclass = function(x, to) {
      new_rsubclass(
        rschedules = x$rschedules,
        rdates = x$rdates,
        exdates = x$exdates,
        foo = to$foo
      )
    }
  )
}
