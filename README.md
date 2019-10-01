
<!-- README.md is generated from README.Rmd. Please edit that file -->

# almanac

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/DavisVaughan/almanac.svg?branch=master)](https://travis-ci.org/DavisVaughan/almanac)
[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/almanac/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/almanac?branch=master)
<!-- badges: end -->

almanac implements a *grammar of schedules*, providing the fundamental
building blocks to construct schedules that identify “events” such as
weekends or holidays. After constructing a schedule, it can be used to:

  - Generate dates that fall in the schedule.

  - Determine if a date is in the schedule or not.

  - Shift a sequence of dates, stepping over dates that fall in the
    schedule.

## Installation

You can NOT install the released version of almanac from
[CRAN](https://CRAN.R-project.org) with:

``` r
# NO! install.packages("almanac")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/almanac")
```
