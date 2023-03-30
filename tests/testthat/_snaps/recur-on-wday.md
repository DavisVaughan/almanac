# can normalize various weekday character strings

    Code
      yearly() %>% recur_on_wday("mond")
    Condition
      Error in `wday_normalize()`:
      ! A character `x` must be a weekday name or abbreviation.

# cannot use `wday > 7` or `wday < 1`

    Code
      (expect_error(yearly() %>% recur_on_wday(8)))
    Output
      <error/rlang_error>
      Error in `recur_on_wday()`:
      ! `wday` must be in [1, 7].
    Code
      (expect_error(yearly() %>% recur_on_wday(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_wday()`:
      ! `wday` must be in [1, 7].
    Code
      (expect_error(yearly() %>% recur_on_wday(-1)))
    Output
      <error/rlang_error>
      Error in `recur_on_wday()`:
      ! `wday` must be in [1, 7].

# wday must be a character / integer

    Code
      yearly() %>% recur_on_wday(30.5)
    Condition
      Error in `recur_on_wday()`:
      ! Can't convert from `wday` <double> to <integer> due to loss of precision.
      * Locations: 1

