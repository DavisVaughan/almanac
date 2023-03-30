# rschedule is checked

    Code
      radjusted(1, daily(), adj_none)
    Condition
      Error in `glubort()`:
      ! Input `rschedule` must be an rschedule, such as an rrule or rbundle.

# adjust_on is checked

    Code
      radjusted(daily(), 1, adj_none)
    Condition
      Error in `glubort()`:
      ! Input `adjust_on` must be an rschedule, such as an rrule or rbundle.

# adjustment is checked

    Code
      radjusted(daily(), daily(), 1)
    Condition
      Error in `glubort()`:
      ! Input `adjustment` must be a function.

---

    Code
      radjusted(daily(), daily(), function(x) x)
    Condition
      Error in `glubort()`:
      ! Input `adjustment` must have two arguments, `x` and `rschedule`.

