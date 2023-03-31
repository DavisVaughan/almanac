# can handle NA `from` and `to` values

    Code
      alma_seq(na, Sys.Date(), daily())
    Condition
      Error in `alma_seq()`:
      ! `from` and `to` cannot be `NA`

---

    Code
      alma_seq(Sys.Date(), na, daily())
    Condition
      Error in `alma_seq()`:
      ! `from` and `to` cannot be `NA`

