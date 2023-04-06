# cannot be set twice

    Code
      recur_on_easter(x)
    Condition
      Error in `recur_on_easter()`:
      ! The "easter" rule is already set and can't be set twice.

# `offset` is deprecated

    Code
      recur_on_easter(yearly(), offset = 2)
    Condition
      Warning:
      The `offset` argument of `recur_on_easter()` is deprecated as of almanac 1.0.0.
      i Please use `roffset()` instead.
    Message
      <rrule>
      * frequency: yearly
      * since: 1900-01-01
      * until: 2100-01-01
      * easter: offset = 2

# offset must be integerish

    Code
      recur_on_easter(yearly(), offset = 1.5)
    Condition
      Error in `recur_on_easter()`:
      ! `offset` must be a whole number, not the number 1.5.

# offset cannot be NA

    Code
      recur_on_easter(yearly(), offset = NA)
    Condition
      Error in `recur_on_easter()`:
      ! `offset` must be a whole number, not `NA`.

# offset is bounded

    Code
      recur_on_easter(yearly(), offset = 367)
    Condition
      Error in `recur_on_easter()`:
      ! `offset` must be a whole number between -366 and 366, not the number 367.

---

    Code
      recur_on_easter(yearly(), offset = -367)
    Condition
      Error in `recur_on_easter()`:
      ! `offset` must be a whole number between -366 and 366, not the number -367.

