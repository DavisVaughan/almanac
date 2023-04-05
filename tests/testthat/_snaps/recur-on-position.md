# cannot set the position twice

    Code
      daily() %>% recur_on_position(1) %>% recur_on_position(1)
    Condition
      Error in `recur_on_position()`:
      ! The "position" rule is already set and can't be set twice.

# cannot set the position twice within the same call

    Code
      daily() %>% recur_on_position(c(1, 1))
    Condition
      Error in `recur_on_position()`:
      ! `n` can't contain duplicate values.
      i Duplicate values were detected at location: 1 and 2.

# position is validated depending on the frequency

    Code
      (expect_error(daily() %>% recur_on_position(2)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "daily" frequency, the absolute value of `n` can't be larger than 1.
    Code
      (expect_error(daily() %>% recur_on_position(-2)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "daily" frequency, the absolute value of `n` can't be larger than 1.
    Code
      (expect_error(weekly() %>% recur_on_position(8)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "weekly" frequency, the absolute value of `n` can't be larger than 7.
    Code
      (expect_error(weekly() %>% recur_on_position(-8)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "weekly" frequency, the absolute value of `n` can't be larger than 7.
    Code
      (expect_error(monthly() %>% recur_on_position(32)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "monthly" frequency, the absolute value of `n` can't be larger than 31.
    Code
      (expect_error(monthly() %>% recur_on_position(-32)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "monthly" frequency, the absolute value of `n` can't be larger than 31.
    Code
      (expect_error(yearly() %>% recur_on_position(367)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "yearly" frequency, the absolute value of `n` can't be larger than 366.
    Code
      (expect_error(yearly() %>% recur_on_position(-367)))
    Output
      <error/rlang_error>
      Error in `recur_on_position()`:
      ! For a "yearly" frequency, the absolute value of `n` can't be larger than 366.

# `n` must be castable to an integer

    Code
      yearly() %>% recur_on_position(21.5)
    Condition
      Error in `recur_on_position()`:
      ! Can't convert from `n` <double> to <integer> due to loss of precision.
      * Locations: 1

# `n` can't be missing

    Code
      yearly() %>% recur_on_position(NA_integer_)
    Condition
      Error in `recur_on_position()`:
      ! `n` can't contain missing values.
      i Missing values were detected at locations: 1.

