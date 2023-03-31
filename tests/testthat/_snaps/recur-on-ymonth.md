# can normalize various month character strings

    Code
      yearly() %>% recur_on_ymonth("Janu")
    Condition
      Error in `month_normalize()`:
      ! A character `x` must be a month name or abbreviation.

# cannot use `ymonth > 12` or `ymonth < 1`

    Code
      (expect_error(yearly() %>% recur_on_ymonth(13)))
    Output
      <error/rlang_error>
      Error in `recur_on_ymonth()`:
      ! `ymonth` can only take values in [1, 12].
    Code
      (expect_error(yearly() %>% recur_on_ymonth(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_ymonth()`:
      ! `ymonth` can only take values in [1, 12].
    Code
      (expect_error(yearly() %>% recur_on_ymonth(-1)))
    Output
      <error/rlang_error>
      Error in `recur_on_ymonth()`:
      ! `ymonth` can only take values in [1, 12].

# ymonth must be a character / integer

    Code
      yearly() %>% recur_on_ymonth(30.5)
    Condition
      Error in `recur_on_ymonth()`:
      ! Can't convert from `ymonth` <double> to <integer> due to loss of precision.
      * Locations: 1

