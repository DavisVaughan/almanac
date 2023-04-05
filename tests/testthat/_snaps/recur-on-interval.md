# cannot use `interval < 1`

    Code
      (expect_error(yearly() %>% recur_on_interval(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_interval()`:
      ! `n` must be a whole number larger than or equal to 1, not the number 0.
    Code
      (expect_error(yearly() %>% recur_on_interval(-1)))
    Output
      <error/rlang_error>
      Error in `recur_on_interval()`:
      ! `n` must be a whole number larger than or equal to 1, not the number -1.

# interval must be an integer

    Code
      yearly() %>% recur_on_interval(30.5)
    Condition
      Error in `recur_on_interval()`:
      ! `n` must be a whole number, not the number 30.5.

