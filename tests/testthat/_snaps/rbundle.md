# validates rschedules

    Code
      new_rbundle(1)
    Condition
      Error:
      ! `rschedules` must be a list, not the number 1.

# validates rdates

    Code
      (expect_error(new_rbundle(rdates = 1)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` must be a <Date>, not the number 1.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_neg_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_na_date)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` can't contain missing values.
      i Missing values were detected at locations: 1.

# validates exdates

    Code
      (expect_error(new_rbundle(exdates = 1)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` must be a <Date>, not the number 1.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_neg_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_na_date)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` can't contain missing values.
      i Missing values were detected at locations: 1.

# validates date bounds

    Code
      (expect_error(new_rbundle(rdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error:
      ! `rdates` must be larger than `0100-01-01`.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error:
      ! `rdates` must be smaller than `9999-12-31`.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error:
      ! `exdates` must be larger than `0100-01-01`.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error:
      ! `exdates` must be smaller than `9999-12-31`.

# `...` must be named

    Code
      new_rbundle(rschedules = list(), rdates = new_date(), exdates = new_date(), 1)
    Condition
      Error in `new_rschedule()`:
      ! `...` must have named elements.

