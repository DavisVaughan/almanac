# cannot use `day > 366` or `day < -366` or `day == 0`

    Code
      (expect_error(yearly() %>% recur_on_day_of_year(367)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_year()`:
      ! `day` can only take values in [-366, -1] and [1, 366].
    Code
      (expect_error(yearly() %>% recur_on_day_of_year(-367)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_year()`:
      ! `day` can only take values in [-366, -1] and [1, 366].
    Code
      (expect_error(yearly() %>% recur_on_day_of_year(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_day_of_year()`:
      ! `day` can only take values in [-366, -1] and [1, 366].

# `day` must be an integer

    Code
      yearly() %>% recur_on_day_of_year(367.5)
    Condition
      Error in `recur_on_day_of_year()`:
      ! Can't convert from `day` <double> to <integer> due to loss of precision.
      * Locations: 1

