# print method works well

    Code
      x
    Message
      <The Holiday>
       <rrule>
       * frequency: yearly
       * range: [1900-01-01, 2100-01-01]
       * month of year: Nov
       * day of month: 5

---

    Code
      x
    Message
      <The Holiday>
       <radjusted>
        adjust:
        <rrule>
        * frequency: yearly
        * range: [1900-01-01, 2100-01-01]
        * month of year: Nov
        * day of month: 5
        adjust on:
        <rrule>
        * frequency: weekly
        * range: [1900-01-01, 2100-01-01]
        * day of week: Sat, and Sun

---

    Code
      x
    Message
      <The Holiday>
       <roffset[by = 1]>
        <radjusted>
         adjust:
         <rrule>
         * frequency: yearly
         * range: [1900-01-01, 2100-01-01]
         * month of year: Nov
         * day of month: 5
         adjust on:
         <rrule>
         * frequency: weekly
         * range: [1900-01-01, 2100-01-01]
         * day of week: Sat, and Sun

---

    Code
      x
    Message
      <The Holiday>
       <radjusted>
        adjust:
        <roffset[by = 1]>
         <radjusted>
          adjust:
          <rrule>
          * frequency: yearly
          * range: [1900-01-01, 2100-01-01]
          * month of year: Nov
          * day of month: 5
          adjust on:
          <rrule>
          * frequency: weekly
          * range: [1900-01-01, 2100-01-01]
          * day of week: Sat, and Sun
        adjust on:
        <rrule>
        * frequency: weekly
        * range: [1900-01-01, 2100-01-01]
        * day of week: Sat, and Sun

# `name` is checked

    Code
      rholiday(yearly(), 1)
    Condition
      Error in `rholiday()`:
      ! `name` must be a valid name, not the number 1.

---

    Code
      rholiday(yearly(), "")
    Condition
      Error in `rholiday()`:
      ! `name` must be a valid name, not the empty string "".

---

    Code
      rholiday(yearly(), NA_character_)
    Condition
      Error in `rholiday()`:
      ! `name` must be a valid name, not a character `NA`.

---

    Code
      rholiday(yearly(), c("a", "b"))
    Condition
      Error in `rholiday()`:
      ! `name` must be a valid name, not a character vector.

# `rschedule` is checked

    Code
      rholiday(1, "a")
    Condition
      Error in `rholiday()`:
      ! `rschedule` must be a <almanac_rschedule>, not the number 1.

# `hol_observe()` validates `x`

    Code
      hol_observe(1, yearly(), adj_nearest)
    Condition
      Error in `hol_observe()`:
      ! `x` must be a <almanac_rholiday>, not the number 1.

# `hol_offset()` validates `x`

    Code
      hol_offset(1, by = 2)
    Condition
      Error in `hol_offset()`:
      ! `x` must be a <almanac_rholiday>, not the number 1.

# `hol_rename()` validates `x`

    Code
      hol_rename(1, "foo")
    Condition
      Error in `hol_rename()`:
      ! `x` must be a <almanac_rholiday>, not the number 1.

# `hol_rename()` validates `name`

    Code
      hol_rename(hol_christmas(), 1)
    Condition
      Error in `hol_rename()`:
      ! `name` must be a valid name, not the number 1.

