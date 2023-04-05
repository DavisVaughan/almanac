# can normalize various weekday character strings

    Code
      yearly() %>% recur_on_day_of_week("mond")
    Condition
      Error in `recur_on_day_of_week()`:
      ! `day` must be a weekday name or abbreviation.

# cannot use `day > 7` or `day < 1`

    Code
      (expect_error(yearly() %>% recur_on_day_of_week(8)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_week()`:
      ! `day` must be in [1, 7].
    Code
      (expect_error(yearly() %>% recur_on_day_of_week(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_week()`:
      ! `day` must be in [1, 7].
    Code
      (expect_error(yearly() %>% recur_on_day_of_week(-1)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_week()`:
      ! `day` must be in [1, 7].

# `day` must be a character / integer

    Code
      yearly() %>% recur_on_day_of_week(30.5)
    Condition
      Error in `recur_on_day_of_week()`:
      ! Can't convert from `day` <double> to <integer> due to loss of precision.
      * Locations: 1

# `day` can't be missing

    Code
      yearly() %>% recur_on_day_of_week(NA_integer_)
    Condition
      Error in `recur_on_day_of_week()`:
      ! `day` can't contain missing values.
      i Missing values were detected at locations: 1.

# `nth` can't be missing

    Code
      yearly() %>% recur_on_day_of_week(1, nth = NA_integer_)
    Condition
      Error in `recur_on_day_of_week()`:
      ! `nth` can't contain missing values.
      i Missing values were detected at locations: 1.

