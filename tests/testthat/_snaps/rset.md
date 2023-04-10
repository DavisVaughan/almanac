# validates rschedules

    Code
      new_rset(1)
    Condition
      Error:
      ! `rschedules` must be a list, not the number 1.

# validates rdates

    Code
      (expect_error(new_rset(rdates = 1)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` must be a <Date>, not the number 1.
    Code
      (expect_error(new_rset(rdates = almanac_global_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rset(rdates = almanac_global_neg_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rset(rdates = almanac_global_na_date)))
    Output
      <error/rlang_error>
      Error:
      ! `rdates` can't contain missing values.
      i Missing values were detected at locations: 1.

# validates exdates

    Code
      (expect_error(new_rset(exdates = 1)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` must be a <Date>, not the number 1.
    Code
      (expect_error(new_rset(exdates = almanac_global_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rset(exdates = almanac_global_neg_inf_date)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` can't contain infinite values.
      i Infinite values were detected at locations: 1.
    Code
      (expect_error(new_rset(exdates = almanac_global_na_date)))
    Output
      <error/rlang_error>
      Error:
      ! `exdates` can't contain missing values.
      i Missing values were detected at locations: 1.

# validates date bounds

    Code
      (expect_error(new_rset(rdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error:
      ! `rdates` must be larger than `0100-01-01`.
    Code
      (expect_error(new_rset(rdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error:
      ! `rdates` must be smaller than `9999-12-31`.
    Code
      (expect_error(new_rset(exdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error:
      ! `exdates` must be larger than `0100-01-01`.
    Code
      (expect_error(new_rset(exdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error:
      ! `exdates` must be smaller than `9999-12-31`.

# `...` must be named

    Code
      new_rset(rschedules = list(), rdates = new_date(), exdates = new_date(), 1)
    Condition
      Error in `new_rschedule()`:
      ! `...` must have named elements.

# runion() generates informative output

    Code
      # # Empty runion
      runion()
    Message
      <runion[0]>
    Code
      # # With rschedules
      runion(daily(), yearly())
    Message
      <runion[2]>
       <rrule>
       * frequency: daily
       * range: [1900-01-01, 2100-01-01]
       <rrule>
       * frequency: yearly
       * range: [1900-01-01, 2100-01-01]

# rintersect() generates informative output

    Code
      # # Empty rintersect
      rintersect()
    Message
      <rintersect[0]>
    Code
      # # With rschedules
      rintersect(daily(), yearly())
    Message
      <rintersect[2]>
       <rrule>
       * frequency: daily
       * range: [1900-01-01, 2100-01-01]
       <rrule>
       * frequency: yearly
       * range: [1900-01-01, 2100-01-01]

# rsetdiff() generates informative output

    Code
      # # Empty rsetdiff
      rsetdiff()
    Message
      <rsetdiff[0]>
    Code
      # # With rschedules
      rsetdiff(daily(), yearly())
    Message
      <rsetdiff[2]>
       <rrule>
       * frequency: daily
       * range: [1900-01-01, 2100-01-01]
       <rrule>
       * frequency: yearly
       * range: [1900-01-01, 2100-01-01]

