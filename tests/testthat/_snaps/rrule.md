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
      Error in `check_since()`:
      ! `since` must be larger than `0100-01-01`.

---

    Code
      daily(until = until + 1)
    Condition
      Error in `check_until()`:
      ! `until` must be smaller than `9999-12-31`.

# `check_rrule()` works

    Code
      check_rrule(1)
    Condition
      Error:
      ! `1` must be a <rrule>, not the number 1.

---

    Code
      check_rrule(1, allow_null = TRUE)
    Condition
      Error:
      ! `1` must be a <rrule> or `NULL`, not the number 1.

