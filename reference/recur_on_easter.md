# Recur on easter

`recur_on_easter()` is a special helper to recur on Easter. Easter is
particularly difficult to construct a recurrence rule for.

## Usage

``` r
recur_on_easter(x, offset = NULL)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- offset:

  **\[deprecated\]**

  `[integer(1)]`

  Deprecated in favor of using [`roffset()`](roffset.md) directly.

  An offset in terms of a number of days on either side of Easter to
  recur on. This offset must still fall within the same year, otherwise
  the date will be silently ignored.

## Value

An updated rrule.

## Examples

``` r
on_easter <- yearly() %>% recur_on_easter()

# Rather than:
if (FALSE) {
on_easter_monday <- yearly() %>% recur_on_easter(1)
}

# Please use:
on_easter_monday <- roffset(on_easter, 1)

alma_search("1999-01-01", "2001-01-01", on_easter)
#> [1] "1999-04-04" "2000-04-23"

both <- runion(on_easter, on_easter_monday)

alma_search("1999-01-01", "2001-01-01", both)
#> [1] "1999-04-04" "1999-04-05" "2000-04-23" "2000-04-24"
```
