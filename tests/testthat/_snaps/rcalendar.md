# print method shows holiday names

    Code
      x
    Message
      <rcalendar[2]>
      * Christmas
      * Easter

---

    Code
      runion(x, x)
    Message
      <runion[2]>
       <rcalendar[2]>
       * Christmas
       * Easter
       <rcalendar[2]>
       * Christmas
       * Easter

# can create an empty calendar

    Code
      x
    Message
      <rcalendar[0]>

# checks inputs

    Code
      rcalendar(1)
    Condition
      Error in `rcalendar()`:
      ! `rholidays[[1]]` must be a <almanac_rholiday>, not the number 1.

# inputs must have unique holiday names

    Code
      rcalendar(x, x)
    Condition
      Error in `rcalendar()`:
      ! Can't supply duplicate holidays.
      i The name "Christmas" is duplicated.

---

    Code
      rcalendar(x, y)
    Condition
      Error in `rcalendar()`:
      ! Can't supply duplicate holidays.
      i The name "Christmas" is duplicated.

# `cal_names()` validates its input

    Code
      cal_names(1)
    Condition
      Error in `cal_names()`:
      ! `x` must be a <almanac_rcalendar>, not the number 1.

# `cal_events()` validates `x`

    Code
      cal_events(1)
    Condition
      Error in `cal_events()`:
      ! `x` must be a <almanac_rcalendar>, not the number 1.

# `cal_events()` validates `year`

    Code
      cal_events(rcalendar(), year = "x")
    Condition
      Error in `cal_events()`:
      ! Can't convert `year` <character> to <integer>.

---

    Code
      cal_events(rcalendar(), year = NA_integer_)
    Condition
      Error in `cal_events()`:
      ! `year` can't contain missing values.
      i Missing values were detected at locations: 1.

# `cal_events()` validates `observed`

    Code
      cal_events(rcalendar(), observed = 1)
    Condition
      Error in `cal_events()`:
      ! `observed` must be `TRUE` or `FALSE`, not the number 1.

# `cal_match()` validates `x`

    Code
      cal_match(1, rcalendar())
    Condition
      Error in `cal_match()`:
      ! Can't convert `x` <double> to <date>.

# `cal_match()` validates `rcalendar`

    Code
      cal_match(x, 1)
    Condition
      Error in `cal_match()`:
      ! `rcalendar` must be a <almanac_rcalendar>, not the number 1.

# can't add duplicate holiday

    Code
      cal_add(x, hol_christmas())
    Condition
      Error in `cal_add()`:
      ! Can't add a holiday that already exists in the calendar.
      i "Christmas" already exists in the calendar.

# `cal_add()` validates `x`

    Code
      cal_add(1, hol_christmas())
    Condition
      Error in `cal_add()`:
      ! `x` must be a <almanac_rcalendar>, not the number 1.

# `cal_add()` validates `rholiday`

    Code
      cal_add(rcalendar(), 1)
    Condition
      Error in `cal_add()`:
      ! `rholiday` must be a <almanac_rholiday>, not the number 1.

# can't remove holiday that doesn't exist

    Code
      cal_remove(x, "Christmas")
    Condition
      Error in `cal_remove()`:
      ! Can't remove a holiday that isn't in the calendar.
      i "Christmas" isn't in the calendar.

---

    Code
      cal_remove(x, hol_new_years_day())
    Condition
      Error in `cal_remove()`:
      ! Can't remove a holiday that isn't in the calendar.
      i "New Year's Day" isn't in the calendar.

# `cal_remove()` validates `x`

    Code
      cal_remove(1, "Christmas")
    Condition
      Error in `cal_remove()`:
      ! `x` must be a <almanac_rcalendar>, not the number 1.

# `cal_remove()` validates `what`

    Code
      cal_remove(rcalendar(), 1)
    Condition
      Error in `cal_remove()`:
      ! `what` must be a <almanac_rholiday>, not the number 1.

