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

# radjusted has informative print method

    Code
      # # basic method
      radjusted(daily(), daily(), adj_none)
    Message
      <radjusted>
       adjust:
       <rrule>
       * frequency: daily
       * since: 1900-01-01
       * until: 2100-01-01
       adjust on:
       <rrule>
       * frequency: daily
       * since: 1900-01-01
       * until: 2100-01-01
    Code
      # # with runions
      rrule <- recur_on_day_of_week(weekly(), "Wed")
      runion <- add_rschedule(runion(), weekly())
      radjusted(rrule, runion, adj_none)
    Message
      <radjusted>
       adjust:
       <rrule>
       * frequency: weekly
       * since: 1900-01-01
       * until: 2100-01-01
       * day of week: Wed
       adjust on:
       <runion[1]>
        <rrule>
        * frequency: weekly
        * since: 1900-01-01
        * until: 2100-01-01

