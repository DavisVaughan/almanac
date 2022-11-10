# almanac (development version)

* R >=3.4.0 is now required, which is in line with tidyverse standards.

# almanac 0.1.1

* Explicitly imports `R6::R6Class()` and `V8::v8()` to avoid R CMD Check
  false alarms (#74).

* Fix USBAN error of casting `NA_real_` and `NaN` to integer (#72).

# almanac 0.1.0

* Added a `NEWS.md` file to track changes to the package.
