# cannot use `mday > 31` or `mday < -31` or `mday == 0`

    Code
      (expect_error(yearly() %>% recur_on_mday(32)))
    Output
      <error/rlang_error>
      Error in `recur_on_mday()`:
      ! `mday` can only take values in [-31, -1] and [1, 31].
    Code
      (expect_error(yearly() %>% recur_on_mday(-32)))
    Output
      <error/rlang_error>
      Error in `recur_on_mday()`:
      ! `mday` can only take values in [-31, -1] and [1, 31].
    Code
      (expect_error(yearly() %>% recur_on_mday(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_mday()`:
      ! `mday` can only take values in [-31, -1] and [1, 31].

# mday must be an integer

    Code
      yearly() %>% recur_on_mday(30.5)
    Condition
      Error in `recur_on_mday()`:
      ! Can't convert from `mday` <double> to <integer> due to loss of precision.
      * Locations: 1

