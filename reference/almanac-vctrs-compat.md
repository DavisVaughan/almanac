# vctrs compatibility functions

These functions are the extensions that allow stepper objects to work
with vctrs.

## Usage

``` r
# S3 method for class 'almanac_stepper'
vec_arith(op, x, y, ...)

# S3 method for class 'almanac_stepper.almanac_stepper'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'almanac_stepper.almanac_stepper'
vec_cast(x, to, ..., x_arg = "", to_arg = "")
```

## Arguments

- op:

  An arithmetic operator as a string.

- x, y, to:

  Objects.

- ...:

  Used to pass along error message information.

- x_arg, y_arg, to_arg:

  Used to pass along error message information.

## Value

See the corresponding vctrs function for the exact return value.

## Examples

``` r
library(vctrs)
vec_arith("+", as.Date("2019-01-04"), workdays(1))
#> [1] "2019-01-07"
```
