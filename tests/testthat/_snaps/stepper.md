# must be integerish `n`

    Code
      step(1.5)
    Condition
      Error in `step()`:
      ! Can't convert from `n` <double> to <integer> due to loss of precision.
      * Locations: 1

# `n` is validated

    Code
      new_stepper(1)
    Condition
      Error in `new_stepper()`:
      ! `n` must be an integer.

# `rschedule` is validated

    Code
      new_stepper(rschedule = 1)
    Condition
      Error in `glubort()`:
      ! Input `rschedule` must be an rschedule, such as an rrule or rbundle.

# default method error is thrown

    Code
      (expect_error(vec_arith("+", new_stepper(), 1), class = "vctrs_error_incompatible_op")
      )
    Output
      <error/vctrs_error_incompatible_op>
      Error in `vec_arith()`:
      ! <stepper> + <double> is not permitted
    Code
      (expect_error(vec_arith("+", 1, new_stepper()), class = "vctrs_error_incompatible_op")
      )
    Output
      <error/vctrs_error_incompatible_op>
      Error in `vec_arith()`:
      ! <double> + <stepper> is not permitted

# cannot subtract date from stepper

    Code
      step(1) %s-% x
    Condition
      Error in `vec_arith()`:
      ! <stepper> - <date> is not permitted

# steppers are coercible if from the same rschedule

    Code
      vec_ptype2(x, new_stepper())
    Condition
      Error in `vec_ptype2.almanac_stepper.almanac_stepper()`:
      ! Can't combine `x` <stepper> and `new_stepper()` <stepper>.
      Steppers must have identical rschedules to be coercible.

---

    Code
      vec_cast(x, new_stepper())
    Condition
      Error in `vec_cast.almanac_stepper.almanac_stepper()`:
      ! Can't convert `x` <stepper> to <stepper>.
      Steppers must have identical rschedules to be coercible.

