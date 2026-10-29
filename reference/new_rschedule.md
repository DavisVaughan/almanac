# Create a new rschedule

`new_rschedule()` is a developer focused tool that is not required for
normal usage of almanac. It is only exported to allow other packages to
construct new rschedule objects that work with almanac functions
prefixed with `alma_*()`, like [`alma_in()`](alma_in.md).

`rschedule_events()` is a generic function that rschedule subclasses
must provide a method for. `rschedule_events()` should return a Date
vector containing the complete ordered set of events in the event set of
that rschedule.

## Usage

``` r
new_rschedule(..., class)

rschedule_events(x)
```

## Arguments

- ...:

  `[named fields]`

  Named data fields.

- class:

  `[character]`

  A required subclass.

- x:

  `[rschedule subclass]`

  An object that subclasses rschedule.

## Value

For `new_rschedule()`, a new rschedule subclass.

For `rschedule_events()`, a Date vector of events.

## Details

An rschedule is an abstract class that rrule and rset both inherit from.
The sole functionality of rschedule classes is to provide a method for
`rschedule_events()`.

## Examples

``` r
events <- as.Date("1970-01-01")

static <- new_rschedule(
  events = events,
  class = "static_rschedule"
)

# You have to register an `rschedule_events()` method first!
try(alma_events(static))
#> Error in rschedule_events(rschedule) : 
#>   <almanac_rschedule> subclasses must provide their own
#> `rschedule_events()` method.
```
