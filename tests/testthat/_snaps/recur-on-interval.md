# cannot use `interval < 1`

    Code
      (expect_error(yearly() %>% recur_on_interval(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_interval()`:
      ! `n` must be greater than 0.
    Code
      (expect_error(yearly() %>% recur_on_interval(-1)))
    Output
      <error/rlang_error>
      Error in `recur_on_interval()`:
      ! `n` must be greater than 0.

# interval must be an integer

    Code
      yearly() %>% recur_on_interval(30.5)
    Condition
      Error in `recur_on_interval()`:
      ! Can't convert from `n` <double> to <integer> due to loss of precision.
      * Locations: 1

