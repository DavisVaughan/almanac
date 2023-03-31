# cannot use `yday > 366` or `yday < -366` or `yday == 0`

    Code
      (expect_error(yearly() %>% recur_on_yday(367)))
    Output
      <error/rlang_error>
      Error in `recur_on_yday()`:
      ! `yday` can only take values in [-366, -1] and [1, 366].
    Code
      (expect_error(yearly() %>% recur_on_yday(-367)))
    Output
      <error/rlang_error>
      Error in `recur_on_yday()`:
      ! `yday` can only take values in [-366, -1] and [1, 366].
    Code
      (expect_error(yearly() %>% recur_on_yday(0)))
    Output
      <error/rlang_error>
      Error in `recur_on_yday()`:
      ! `yday` can only take values in [-366, -1] and [1, 366].

# yday must be an integer

    Code
      yearly() %>% recur_on_yday(367.5)
    Condition
      Error in `recur_on_yday()`:
      ! Can't convert from `yday` <double> to <integer> due to loss of precision.
      * Locations: 1

