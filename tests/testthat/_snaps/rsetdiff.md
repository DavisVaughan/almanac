# rsetdiff() generates informative output

    Code
      # # Empty rsetdiff
      rsetdiff()
    Message
      <rsetdiff[0]>
    Code
      # # With rschedules
      rsetdiff(daily(), yearly())
    Message
      <rsetdiff[2]>
       <rrule>
       * frequency: daily
       * since: 1900-01-01
       * until: 2100-01-01
       <rrule>
       * frequency: yearly
       * since: 1900-01-01
       * until: 2100-01-01

