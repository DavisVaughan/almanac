# can handle NA `from` and `to` values

    Code
      alma_search(na, Sys.Date(), daily())
    Condition
      Error in `alma_search()`:
      ! `from` can't contain missing values.
      i Missing values were detected at locations: 1.

---

    Code
      alma_search(Sys.Date(), na, daily())
    Condition
      Error in `alma_search()`:
      ! `to` can't contain missing values.
      i Missing values were detected at locations: 1.

