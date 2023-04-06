# must supply at least one field

    Code
      new_rschedule(class = "foo")
    Condition
      Error in `new_rschedule()`:
      ! `...` must have at least one field.

# must supply named fields

    Code
      new_rschedule(1, "foo")
    Condition
      Error in `new_rschedule()`:
      ! `...` must have named elements.

# default method errors

    Code
      rschedule_events(1)
    Condition
      Error in `rschedule_events()`:
      ! Can't extract events from a <numeric>.

# method is required for subclasses

    Code
      rschedule_events(x)
    Condition
      Error in `rschedule_events()`:
      ! <almanac_rschedule> subclasses must provide their own `rschedule_events()` method.

