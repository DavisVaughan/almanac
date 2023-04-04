# print method for rrule is informative

    Code
      # # basic method
      daily()
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
    Code
      yearly()
    Message
      <rrule>
      * frequency: yearly
      * since: 1900-01-01
      * until: 2100-01-01
    Code
      # # until is overriden by recur_for_count()
      recur_for_count(daily(), 5)
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * count: 5
    Code
      # # can add multiple conditions
      recur_on_interval(recur_for_count(yearly(), 5), 2)
    Message
      <rrule>
      * frequency: yearly
      * since: 1900-01-01
      * count: 5
      * interval: 2
    Code
      # # can use multiple months of the year
      recur_on_month_of_year(daily(), c("Feb", "Mar"))
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * month of year: Feb and Mar
    Code
      # # can use multiple weeks of the year
      recur_on_week_of_year(daily(), c(5, 9, 12))
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * week of year: 5, 9, and 12
    Code
      # # can use multiple days of the year
      recur_on_day_of_year(daily(), c(5, 9, 12))
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * day of year: 5, 9, and 12
    Code
      # # can use multiple days of the month
      recur_on_day_of_month(daily(), c(5, 9, 12))
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * day of month: 5, 9, and 12
    Code
      # # can use day of week variations
      recur_on_day_of_week(daily(), c("Mon", "Thu"), nth = c(1, 2))
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * day of week: Mon[1, 2], and Thu[1, 2]
    Code
      recur_on_day_of_week(recur_on_day_of_week(daily(), "Mon", nth = c(1, 2)), "Thu",
      nth = c(4, 5))
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * day of week: Mon[1, 2], and Thu[4, 5]
    Code
      recur_on_day_of_week(yearly(), "Mon", nth = c(1, 2, 10, 13, 15, 16))
    Message
      <rrule>
      * frequency: yearly
      * since: 1900-01-01
      * until: 2100-01-01
      * day of week: Mon[1, 2, 10, 13, 15, 16]
    Code
      # # can use multiple positions
      recur_on_position(weekly(), c(-1, 2, 3, -2))
    Message
      <rrule>
      * frequency: weekly
      * since: 1900-01-01
      * until: 2100-01-01
      * position: -2, -1, 2, and 3
    Code
      recur_on_position(yearly(), c(-1, 2, 3, -2, 10, 12, 13))
    Message
      <rrule>
      * frequency: yearly
      * since: 1900-01-01
      * until: 2100-01-01
      * position: -2, -1, 2, 3, 10, 12, and 13
    Code
      # # can change offset
      recur_on_easter(weekly(), offset = -1)
    Message
      <rrule>
      * frequency: weekly
      * since: 1900-01-01
      * until: 2100-01-01
      * easter: offset = -1
    Code
      # # each recur_ condition works
      recur_for_count(daily(), 5)
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * count: 5
    Code
      recur_on_interval(daily(), 5)
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * interval: 5
    Code
      recur_with_week_start(daily(), "Tuesday")
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * week start: Tue
    Code
      recur_on_month_of_year(daily(), "Feb")
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * month of year: Feb
    Code
      recur_on_week_of_year(daily(), 5)
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * week of year: 5
    Code
      recur_on_day_of_year(daily(), 5)
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * day of year: 5
    Code
      recur_on_day_of_month(daily(), 5)
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * day of month: 5
    Code
      recur_on_day_of_week(daily(), "Wed")
    Message
      <rrule>
      * frequency: daily
      * since: 1900-01-01
      * until: 2100-01-01
      * day of week: Wed
    Code
      recur_on_position(weekly(), 5)
    Message
      <rrule>
      * frequency: weekly
      * since: 1900-01-01
      * until: 2100-01-01
      * position: 5
    Code
      recur_on_easter(weekly())
    Message
      <rrule>
      * frequency: weekly
      * since: 1900-01-01
      * until: 2100-01-01
      * easter: offset = 0

