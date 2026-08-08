# Create a recurring holiday

`rholiday()` is used to create custom holidays. It wraps up a holiday
`name` and its corresponding `rschedule` into a holiday object with
special properties.

Holiday objects can be tweaked with
[`hol_rename()`](holiday-utilities.md),
[`hol_observe()`](holiday-utilities.md), and
[`hol_offset()`](holiday-utilities.md), and they can be added to a
calendar with [`rcalendar()`](rcalendar.md).

## Usage

``` r
rholiday(rschedule, name)
```

## Arguments

- rschedule:

  `[rschedule]`

  The recurrence schedule that determines when the holiday occurs.

- name:

  `[character(1)]`

  The name of the holiday. This serves as a unique identifier when
  adding multiple holidays to an [`rcalendar()`](rcalendar.md).

## Examples

``` r
on_christmas <- yearly() %>%
  recur_on_month_of_year("Dec") %>%
  recur_on_day_of_month(25)

# Bundle a holiday name with its recurrence schedule to create a holiday
rholiday(on_christmas, "Christmas")
#> <Christmas>
#>  <rrule>
#>  • frequency: yearly
#>  • range: [1900-01-01, 2100-01-01]
#>  • month of year: Dec
#>  • day of month: 25

# This is how the built in holiday objects are created
hol_christmas()
#> <Christmas>
#>  <rrule>
#>  • frequency: yearly
#>  • range: [1900-01-01, 2100-01-01]
#>  • month of year: Dec
#>  • day of month: 25
```
