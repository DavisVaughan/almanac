# print method is nice

    Code
      x
    Message
      <rcustom[2]>
      * 2019-01-01
      * 2019-02-05

---

    Code
      y
    Message
      <rcustom[100]>
      * 2019-01-02
      * 2019-01-03
      * 2019-01-04
      * 2019-01-05
      * 2019-01-06
      * and 95 more

---

    Code
      z
    Message
      <runion[3]>
       <rcustom[2]>
       * 2019-01-01
       * 2019-02-05
       <rcustom[100]>
       * 2019-01-02
       * 2019-01-03
       * 2019-01-04
       * 2019-01-05
       * 2019-01-06
       * and 95 more
       <rrule>
       * frequency: yearly
       * since: 1900-01-01
       * until: 2100-01-01

# `events` can't be missing

    Code
      rcustom(almanac_global_na_date)
    Condition
      Error in `rcustom()`:
      ! `events` can't contain missing values.
      i Missing values were detected at locations: 1.

---

    Code
      rcustom(almanac_global_nan_date)
    Condition
      Error in `rcustom()`:
      ! `events` can't contain missing values.
      i Missing values were detected at locations: 1.

# `events` must be finite

    Code
      rcustom(almanac_global_inf_date)
    Condition
      Error in `rcustom()`:
      ! `events` can't contain infinite values.
      i Infinite values were detected at locations: 1.

# `events` are cast to date

    Code
      rcustom("2019-01-01")
    Message
      <rcustom[1]>
      * 2019-01-01

---

    Code
      rcustom("2019")
    Condition
      Error in `rcustom()`:
      ! Failed to parse `events` to <Date> at location: 1.

---

    Code
      rcustom(1)
    Condition
      Error in `rcustom()`:
      ! Can't convert `events` <double> to <date>.

