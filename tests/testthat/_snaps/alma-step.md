# tidy recycling rules are used between `x` and `adjustment`

    Code
      alma_step(new_date(c(1, 2)), 1:3, runion())
    Condition
      Error in `alma_step()`:
      ! Can't recycle `x` (size 2) to match `n` (size 3).

# `Inf` `n` is an error

    Code
      alma_step(new_date(c(1, 2)), Inf, runion())
    Condition
      Error in `alma_step()`:
      ! Can't convert from `n` <double> to <integer> due to loss of precision.
      * Locations: 1

