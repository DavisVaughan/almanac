# can catch lossy character to Date cast

    Code
      vec_cast_date("2019-02-31")
    Condition
      Error in `stop_almanac()`:
      ! Failed to parse `x` to Date at location: 1.

# cannot cast integer or double to Date

    Code
      vec_cast_date(1)
    Condition
      Error in `vec_cast_date()`:
      ! Can't convert `x` <double> to <date>.

---

    Code
      vec_cast_date(1L)
    Condition
      Error in `vec_cast_date()`:
      ! Can't convert `x` <integer> to <date>.

# can cast POSIXct with no time to Date

    Code
      vec_cast_date(as.POSIXct("2019-01-01 01:01:01", "UTC"))
    Condition
      Error in `date_cast()`:
      ! Can't convert from `x` <datetime<UTC>> to <date> due to loss of precision.
      * Locations: 1

