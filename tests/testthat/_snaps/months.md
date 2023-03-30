# check lower boundary

    Code
      test_month_from_days(x - 1)
    Condition
      Error:
      ! Minimum date value reached. Cannot compute civil months.

# check upper boundary

    Code
      test_month_from_days(x + 1)
    Condition
      Error:
      ! Maximum date value reached. Cannot compute civil months.

