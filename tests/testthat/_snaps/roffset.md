# print method is nice

    Code
      x
    Message
      <roffset[by = 1]>
       <rrule>
       * frequency: yearly
       * since: 1900-01-01
       * until: 2100-01-01

---

    Code
      z
    Message
      <runion[2]>
       <roffset[by = 1]>
        <rrule>
        * frequency: yearly
        * since: 1900-01-01
        * until: 2100-01-01
       <roffset[by = 1]>
        <rrule>
        * frequency: yearly
        * since: 1900-01-01
        * until: 2100-01-01

# `by` must be a single integer

    Code
      roffset(yearly(), by = "a")
    Condition
      Error in `roffset()`:
      ! `by` must be a whole number, not the string "a".

---

    Code
      roffset(yearly(), by = 1:2)
    Condition
      Error in `roffset()`:
      ! `by` must be a whole number, not an integer vector.

---

    Code
      roffset(yearly(), by = NA_integer_)
    Condition
      Error in `roffset()`:
      ! `by` must be a whole number, not an integer `NA`.

---

    Code
      roffset(yearly(), by = Inf)
    Condition
      Error in `roffset()`:
      ! `by` must be a whole number, not `Inf`.

# `rschedule` is validated

    Code
      roffset(1, by = 2)
    Condition
      Error in `roffset()`:
      ! `rschedule` must be a <almanac_rschedule>, not the number 1.

