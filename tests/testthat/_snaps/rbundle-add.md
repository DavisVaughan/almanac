# errors on max/min rdates and exdates

    Code
      expect_error(add_rdates(runion(), lb), NA)
      (expect_error(add_rdates(runion(), lb - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error in `new_rbundle()`:
      ! `rdates` must be larger than `0100-01-01`.
    Code
      expect_error(add_exdates(runion(), lb), NA)
      (expect_error(add_exdates(runion(), lb - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error in `new_rbundle()`:
      ! `exdates` must be larger than `0100-01-01`.
    Code
      expect_error(add_rdates(runion(), ub), NA)
      (expect_error(add_rdates(runion(), ub + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error in `new_rbundle()`:
      ! `rdates` must be smaller than `9999-12-31`.
    Code
      expect_error(add_exdates(runion(), ub), NA)
      (expect_error(add_exdates(runion(), ub + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error in `new_rbundle()`:
      ! `exdates` must be smaller than `9999-12-31`.

