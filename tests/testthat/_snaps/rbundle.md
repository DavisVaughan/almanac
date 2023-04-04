# validates rschedules

    Code
      new_rbundle(1)
    Condition
      Error in `new_rbundle()`:
      ! `rschedules` must be a list.

---

    Code
      new_rbundle(list(1))
    Condition
      Error in `new_rbundle()`:
      ! `rschedules[[1]]` must be a <rschedule>, not the number 1.

# validates rdates

    Code
      (expect_error(new_rbundle(rdates = 1)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `rdates` must be a Date.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_inf_date)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `rdates` must be finite.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_neg_inf_date)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `rdates` must be finite.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_na_date)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `rdates` must be finite.

# validates exdates

    Code
      (expect_error(new_rbundle(exdates = 1)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `exdates` must be a Date.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_inf_date)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `exdates` must be finite.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_neg_inf_date)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `exdates` must be finite.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_na_date)))
    Output
      <error/rlang_error>
      Error in `new_rbundle()`:
      ! `exdates` must be finite.

# validates date bounds

    Code
      (expect_error(new_rbundle(rdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error in `new_rbundle()`:
      ! `rdates` must be larger than `0100-01-01`.
    Code
      (expect_error(new_rbundle(rdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error in `new_rbundle()`:
      ! `rdates` must be smaller than `9999-12-31`.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error in `new_rbundle()`:
      ! `exdates` must be larger than `0100-01-01`.
    Code
      (expect_error(new_rbundle(exdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error in `new_rbundle()`:
      ! `exdates` must be smaller than `9999-12-31`.

# `...` must be named

    Code
      new_rbundle(rschedules = list(), rdates = new_date(), exdates = new_date(), 1)
    Condition
      Error in `new_rschedule()`:
      ! `...` must have named elements.

# rbundle_restore() gives developers a way to restore to `to`

    Code
      rbundle_restore(x, to)
    Condition
      Error in `rbundle_restore()`:
      ! <rbundle> subclasses must provide their own `rbundle_restore()` method.

