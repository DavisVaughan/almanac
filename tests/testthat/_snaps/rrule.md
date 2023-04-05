# `since` cannot be `NA`

    Code
      daily(since = as.Date(NA))
    Condition
      Error in `daily()`:
      ! `since` can't contain missing values.
      i Missing values were detected at locations: 1.

# `until` cannot be `NA`

    Code
      daily(until = as.Date(NA))
    Condition
      Error in `daily()`:
      ! `until` can't contain missing values.
      i Missing values were detected at locations: 1.

# `since` must be before `until`

    Code
      daily(since = "1970-01-02", until = "1970-01-01")
    Condition
      Error in `daily()`:
      ! `since` must be before `until`.

# errors on max/min dates

    Code
      daily(since = since - 1)
    Condition
      Error in `daily()`:
      ! `since` must be larger than `0100-01-01`.

---

    Code
      daily(until = until + 1)
    Condition
      Error in `daily()`:
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

