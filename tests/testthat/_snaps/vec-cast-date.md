# can catch lossy character to Date cast

    Code
      vec_cast_date("2019-02-31")
    Condition
      Error:
      ! Failed to parse `"2019-02-31"` to <Date> at location: 1.

# cannot cast integer or double to Date

    Code
      vec_cast_date(1)
    Condition
      Error:
      ! Can't convert `1` <double> to <date>.

---

    Code
      vec_cast_date(1L)
    Condition
      Error:
      ! Can't convert `1L` <integer> to <date>.

# can cast POSIXct with no time to Date

    Code
      vec_cast_date(as.POSIXct("2019-01-01 01:01:01", "UTC"))
    Condition
      Error in `date_cast()`:
      ! Can't convert from `as.POSIXct("2019-01-01 01:01:01", "UTC")` <datetime<UTC>> to <date> due to loss of precision.
      * Locations: 1

