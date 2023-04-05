# `count` can only be set once

    Code
      daily() %>% recur_for_count(2) %>% recur_for_count(2)
    Condition
      Error in `recur_for_count()`:
      ! The "count" rule is already set and can't be set twice.

# `count` must be castable to a scalar integer

    Code
      (expect_error(daily() %>% recur_for_count("a")))
    Output
      <error/rlang_error>
      Error in `recur_for_count()`:
      ! `n` must be a whole number, not the string "a".
    Code
      (expect_error(daily() %>% recur_for_count(c(1, 2))))
    Output
      <error/rlang_error>
      Error in `recur_for_count()`:
      ! `n` must be a whole number, not a double vector.

