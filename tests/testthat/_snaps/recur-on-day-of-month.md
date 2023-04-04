# cannot use `day > 31` or `day < -31` or `day == 0`

    Code
      (expect_error(yearly() %>% recur_on_day_of_month(32)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_month()`:
      ! `day` can only take values in [-31, -1] and [1, 31].
    Code
      (expect_error(yearly() %>% recur_on_day_of_month(-32)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_month()`:
      ! `day` can only take values in [-31, -1] and [1, 31].
    Code
      (expect_error(yearly() %>% recur_on_day_of_month(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_month()`:
      ! `day` can only take values in [-31, -1] and [1, 31].

# `day` must be an integer

    Code
      yearly() %>% recur_on_day_of_month(30.5)
    Condition
      Error in `recur_on_day_of_month()`:
      ! Can't convert from `day` <double> to <integer> due to loss of precision.
      * Locations: 1

