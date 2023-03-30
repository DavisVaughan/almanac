# `count` can only be set once

    Code
      daily() %>% recur_for_count(2) %>% recur_for_count(2)
    Condition
      Error in `recur_for_count()`:
      ! `count` has already been set for this rrule.

# `count` must be castable to a scalar integer

    Code
      (expect_error(daily() %>% recur_for_count("a"), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `recur_for_count()`:
      ! Can't convert `n` <character> to <integer>.
    Code
      (expect_error(daily() %>% recur_for_count(c(1, 2)), class = "vctrs_error_assert_size")
      )
    Output
      <error/vctrs_error_assert_size>
      Error in `recur_for_count()`:
      ! `n` must have size 1, not size 2.

