# rschedule is checked

    Code
      radjusted(1, daily(), adj_none)
    Condition
      Error in `new_radjusted()`:
      ! `rschedule` must be a <almanac_rschedule>, not the number 1.

# adjust_on is checked

    Code
      radjusted(daily(), 1, adj_none)
    Condition
      Error in `new_radjusted()`:
      ! `adjust_on` must be a <almanac_rschedule>, not the number 1.

# adjustment is checked

    Code
      radjusted(daily(), daily(), 1)
    Condition
      Error in `new_radjusted()`:
      ! `adjustment` must be a function, not the number 1.

---

    Code
      radjusted(daily(), daily(), function(x) x)
    Condition
      Error in `new_radjusted()`:
      ! `adjustment` must have two arguments, `x` and `rschedule`.

# radjusted has informative print method

    Code
      # # basic method
      radjusted(daily(), daily(), adj_none)
    Message
      <radjusted>
       adjust:
       <rrule>
       * frequency: daily
       * range: [1900-01-01, 2100-01-01]
       adjust on:
       <rrule>
       * frequency: daily
       * range: [1900-01-01, 2100-01-01]
    Code
      # # with runions
      rrule <- recur_on_day_of_week(weekly(), "Wed")
      runion <- runion(weekly())
      radjusted(rrule, runion, adj_none)
    Message
      <radjusted>
       adjust:
       <rrule>
       * frequency: weekly
       * range: [1900-01-01, 2100-01-01]
       * day of week: Wed
       adjust on:
       <runion[1]>
        <rrule>
        * frequency: weekly
        * range: [1900-01-01, 2100-01-01]

