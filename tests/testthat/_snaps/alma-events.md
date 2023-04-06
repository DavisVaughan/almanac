# `year` is validated

    Code
      alma_events(yearly(), year = NA_integer_)
    Condition
      Error in `alma_events()`:
      ! `year` can't contain missing values.
      i Missing values were detected at locations: 1.

---

    Code
      alma_events(yearly(), year = 1.5)
    Condition
      Error in `alma_events()`:
      ! Can't convert from `year` <double> to <integer> due to loss of precision.
      * Locations: 1

