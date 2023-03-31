# cannot use `yweek > 53` or `yweek < -53` or `yweek == 0`

    Code
      (expect_error(yearly() %>% recur_on_yweek(54)))
    Output
      <error/rlang_error>
      Error in `recur_on_yweek()`:
      ! `yweek` can only take values in [-53, -1] and [1, 53].
    Code
      (expect_error(yearly() %>% recur_on_yweek(-54)))
    Output
      <error/rlang_error>
      Error in `recur_on_yweek()`:
      ! `yweek` can only take values in [-53, -1] and [1, 53].
    Code
      (expect_error(yearly() %>% recur_on_yweek(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_yweek()`:
      ! `yweek` can only take values in [-53, -1] and [1, 53].

# yweek must be an integer

    Code
      yearly() %>% recur_on_yweek(30.5)
    Condition
      Error in `recur_on_yweek()`:
      ! Can't convert from `yweek` <double> to <integer> due to loss of precision.
      * Locations: 1

