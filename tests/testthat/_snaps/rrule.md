# `since` cannot be `NA`

    Code
      daily(since = as.Date(NA))
    Condition
      Error in `check_since()`:
      ! `since` must be a finite date.

# `until` cannot be `NA`

    Code
      daily(until = as.Date(NA))
    Condition
      Error in `check_until()`:
      ! `until` must be a finite date.

# `since` must be before `until`

    Code
      daily(since = "1970-01-02", until = "1970-01-01")
    Condition
      Error in `rrule()`:
      ! `since` must be before `until`.

# errors on max/min dates

    Code
      daily(since = since - 1)
    Condition
      Error in `stop_almanac()`:
      ! Input `since` cannot be smaller than 0100-01-01.

---

    Code
      daily(until = until + 1)
    Condition
      Error in `stop_almanac()`:
      ! Input `until` cannot be larger than 9999-12-31.
