# cannot use `week > 53` or `week < -53` or `week == 0`

    Code
      (expect_error(yearly() %>% recur_on_week_of_year(54)))
    Output
      <error/rlang_error>
      Error in `recur_on_week_of_year()`:
      ! `week` can only take values in [-53, -1] and [1, 53].
    Code
      (expect_error(yearly() %>% recur_on_week_of_year(-54)))
    Output
      <error/rlang_error>
      Error in `recur_on_week_of_year()`:
      ! `week` can only take values in [-53, -1] and [1, 53].
    Code
      (expect_error(yearly() %>% recur_on_week_of_year(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_week_of_year()`:
      ! `week` can only take values in [-53, -1] and [1, 53].

# `week` must be an integer

    Code
      yearly() %>% recur_on_week_of_year(30.5)
    Condition
      Error in `recur_on_week_of_year()`:
      ! Can't convert from `week` <double> to <integer> due to loss of precision.
      * Locations: 1

