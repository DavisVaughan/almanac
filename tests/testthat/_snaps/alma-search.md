# can handle NA `from` and `to` values

    Code
      alma_search(na, Sys.Date(), daily())
    Condition
      Error in `alma_search()`:
      ! `from` and `to` cannot be `NA`

---

    Code
      alma_search(Sys.Date(), na, daily())
    Condition
      Error in `alma_search()`:
      ! `from` and `to` cannot be `NA`

