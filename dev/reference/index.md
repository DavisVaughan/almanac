# Package index

## Holidays

- [`rholiday()`](rholiday.md) : Create a recurring holiday
- [`hol_observe()`](holiday-utilities.md)
  [`hol_offset()`](holiday-utilities.md)
  [`hol_rename()`](holiday-utilities.md) : Holiday utility functions
- [`hol_christmas()`](holidays.md) [`hol_christmas_eve()`](holidays.md)
  [`hol_easter()`](holidays.md) [`hol_good_friday()`](holidays.md)
  [`hol_halloween()`](holidays.md) [`hol_new_years_day()`](holidays.md)
  [`hol_new_years_eve()`](holidays.md)
  [`hol_st_patricks_day()`](holidays.md)
  [`hol_valentines_day()`](holidays.md)
  [`hol_us_election_day()`](holidays.md)
  [`hol_us_fathers_day()`](holidays.md)
  [`hol_us_independence_day()`](holidays.md)
  [`hol_us_indigenous_peoples_day()`](holidays.md)
  [`hol_us_juneteenth()`](holidays.md)
  [`hol_us_labor_day()`](holidays.md)
  [`hol_us_martin_luther_king_junior_day()`](holidays.md)
  [`hol_us_memorial_day()`](holidays.md)
  [`hol_us_mothers_day()`](holidays.md)
  [`hol_us_presidents_day()`](holidays.md)
  [`hol_us_thanksgiving()`](holidays.md)
  [`hol_us_veterans_day()`](holidays.md) : Holidays

## Calendars

- [`rcalendar()`](rcalendar.md) : Create a recurring calendar
- [`cal_events()`](cal_events.md) : Calendar events
- [`cal_match()`](cal_match.md) : Calendar matching
- [`cal_names()`](cal_names.md) : Calendar names
- [`cal_us_federal()`](cal_us_federal.md) : US federal calendar
- [`cal_add()`](calendar-add-remove.md)
  [`cal_remove()`](calendar-add-remove.md) : Calendar additions and
  removals
- [`cal_next()`](calendar-locations.md)
  [`cal_previous()`](calendar-locations.md) : Calendar locations

## Recurrence rules

- [`daily()`](rrule.md) [`weekly()`](rrule.md) [`monthly()`](rrule.md)
  [`yearly()`](rrule.md) : Create a recurrence rule
- [`recur_for_count()`](recur_for_count.md) : Control the number of
  times to recur
- [`recur_on_day_of_month()`](recur_on_day_of_month.md) : Recur on a day
  of the month
- [`recur_on_day_of_week()`](recur_on_day_of_week.md)
  [`recur_on_weekdays()`](recur_on_day_of_week.md)
  [`recur_on_weekends()`](recur_on_day_of_week.md) : Recur on a day of
  the week
- [`recur_on_day_of_year()`](recur_on_day_of_year.md) : Recur on a day
  of the year
- [`recur_on_easter()`](recur_on_easter.md) : Recur on easter
- [`recur_on_interval()`](recur_on_interval.md) : Recur on an interval
- [`recur_on_month_of_year()`](recur_on_month_of_year.md) : Recur on a
  month of the year
- [`recur_on_position()`](recur_on_position.md) : Recur on a position
  within a frequency
- [`recur_on_week_of_year()`](recur_on_week_of_year.md) : Recur on a
  week of the year
- [`recur_with_week_start()`](recur_with_week_start.md) : Control the
  start of the week

## Recurrence types

- [`runion()`](rset.md) [`rintersect()`](rset.md)
  [`rsetdiff()`](rset.md) : Create a new set-based recurrence schedule
- [`rcustom()`](rcustom.md) : Create a custom rschedule
- [`roffset()`](roffset.md) : Create an offset rschedule
- [`radjusted()`](radjusted.md) : Create an adjusted rschedule

## Recurrence utilities

- [`alma_in()`](alma_in.md) : Check if dates are in an event set
- [`alma_search()`](alma_search.md) : Search for events
- [`alma_seq()`](alma_seq.md) : Generate date sequences
- [`alma_next()`](alma_next.md) [`alma_previous()`](alma_next.md) :
  Generate the next or previous event
- [`alma_events()`](alma_events.md) : Get all events
- [`alma_step()`](alma_step.md) : Step relative to an rschedule
- [`stepper()`](stepper.md) [`` `%s+%` ``](stepper.md)
  [`` `%s-%` ``](stepper.md) [`workdays()`](stepper.md) : Create a new
  stepper

## Adjustments

- [`adj_following()`](adjustments.md)
  [`adj_preceding()`](adjustments.md)
  [`adj_modified_following()`](adjustments.md)
  [`adj_modified_preceding()`](adjustments.md)
  [`adj_nearest()`](adjustments.md) [`adj_none()`](adjustments.md) :
  Date adjustments

## Developer tools

- [`new_rschedule()`](new_rschedule.md)
  [`rschedule_events()`](new_rschedule.md) : Create a new rschedule
- [`almanac_since()`](almanac-defaults.md)
  [`almanac_until()`](almanac-defaults.md) : Default values in almanac

## Compatibility

- [`vec_arith(`*`<almanac_stepper>`*`)`](almanac-vctrs-compat.md)
  [`vec_ptype2(`*`<almanac_stepper.almanac_stepper>`*`)`](almanac-vctrs-compat.md)
  [`vec_cast(`*`<almanac_stepper.almanac_stepper>`*`)`](almanac-vctrs-compat.md)
  : vctrs compatibility functions
