# Default values in almanac

- `almanac_since()` represents the default `since` date used in almanac.
  It defaults to `1900-01-01`, a Monday.

- `almanac_until()` represents the default `until` date used in almanac.
  It defaults to `2100-01-01`, a Friday.

The choice of `since` and `until` are somewhat arbitrary, but should
generate a useful event set range for most rschedules. If you need to
adjust the defaults, then you should supply the `since` and `until`
arguments directly to the rrule generators, like [`yearly()`](rrule.md)
and [`weekly()`](rrule.md).

The `since` default is particularly important for weekly recurrence
rules, where the `since` date represents the anchor point to begin
counting from. See [`recur_on_day_of_week()`](recur_on_day_of_week.md)
for examples of how to adjust this.

## Usage

``` r
almanac_since()

almanac_until()
```

## Examples

``` r
almanac_since()
#> [1] "1900-01-01"
almanac_until()
#> [1] "2100-01-01"
```
