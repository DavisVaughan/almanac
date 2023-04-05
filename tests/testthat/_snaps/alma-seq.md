# can handle NA `from` and `to` values

    Code
      alma_seq(na, Sys.Date(), daily())
    Condition
      Error in `alma_seq()`:
      ! `from` can't contain missing values.
      i Missing values were detected at locations: 1.

---

    Code
      alma_seq(Sys.Date(), na, daily())
    Condition
      Error in `alma_seq()`:
      ! `to` can't contain missing values.
      i Missing values were detected at locations: 1.

