# Calendar additions and removals

- `cal_add()` adds an rholiday to an rcalendar.

- `cal_remove()` removes an rholiday from an rcalendar by name, either
  by specifying a character name or an rholiday object with the same
  name.

## Usage

``` r
cal_add(x, rholiday)

cal_remove(x, what)
```

## Arguments

- x:

  `[rcalendar]`

  An rcalendar.

- rholiday:

  `[rholiday]`

  An rholiday to add to the rcalendar.

- what:

  `[character(1) / rholiday]`

  The name of a holiday to remove from the rcalendar, or an rholiday
  object with the corresponding name that you'd like to remove.

## Value

A new rcalendar with the holiday added or removed.

## Examples

``` r
cal <- rcalendar(
  hol_christmas(),
  hol_halloween(),
  hol_new_years_day(),
  hol_us_presidents_day()
)

# Can't forget Easter!
cal %>%
  cal_add(hol_easter())
#> <rcalendar[5]>
#> • Christmas
#> • Halloween
#> • New Year's Day
#> • US Presidents' Day
#> • Easter

# Didn't actually need Halloween
cal %>%
  cal_remove(hol_halloween())
#> <rcalendar[3]>
#> • Christmas
#> • New Year's Day
#> • US Presidents' Day

# Can remove by name or by object
cal %>%
  cal_remove("Halloween")
#> <rcalendar[3]>
#> • Christmas
#> • New Year's Day
#> • US Presidents' Day
```
