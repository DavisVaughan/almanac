# can normalize various month character strings

    Code
      yearly() %>% recur_on_month_of_year("Janu")
    Condition
      Error in `month_normalize()`:
      ! A character `x` must be a month name or abbreviation.

# cannot use `month > 12` or `month < 1`

    Code
      (expect_error(yearly() %>% recur_on_month_of_year(13)))
    Output
      <error/rlang_error>
      Error in `recur_on_month_of_year()`:
      ! `month` can only take values in [1, 12].
    Code
      (expect_error(yearly() %>% recur_on_month_of_year(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_month_of_year()`:
      ! `month` can only take values in [1, 12].
    Code
      (expect_error(yearly() %>% recur_on_month_of_year(-1)))
    Output
      <error/rlang_error>
      Error in `recur_on_month_of_year()`:
      ! `month` can only take values in [1, 12].

# `month` must be a character / integer

    Code
      yearly() %>% recur_on_month_of_year(30.5)
    Condition
      Error in `recur_on_month_of_year()`:
      ! Can't convert from `month` <double> to <integer> due to loss of precision.
      * Locations: 1

