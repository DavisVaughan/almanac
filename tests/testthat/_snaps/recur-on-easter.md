# cannot be set twice

    Code
      recur_on_easter(x)
    Condition
      Error in `recur_on_easter()`:
      ! The `easter` rule has already been set.

# offset must be integerish

    Code
      recur_on_easter(yearly(), offset = 1.5)
    Condition
      Error in `recur_on_easter()`:
      ! Can't convert from `offset` <double> to <integer> due to loss of precision.
      * Locations: 1

# offset cannot be NA

    Code
      recur_on_easter(yearly(), offset = NA)
    Condition
      Error in `recur_on_easter()`:
      ! `offset` cannot be `NA`.

# offset is bounded

    Code
      recur_on_easter(yearly(), offset = 367)
    Condition
      Error in `recur_on_easter()`:
      ! `offset` can only take values in [-366, 366].

---

    Code
      recur_on_easter(yearly(), offset = -367)
    Condition
      Error in `recur_on_easter()`:
      ! `offset` can only take values in [-366, 366].

