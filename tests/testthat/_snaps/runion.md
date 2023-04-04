# runion() generates informative output

    Code
      # # Empty runion
      runion()
    Message
      <runion[0]>
    Code
      # # With rschedules
      runion() %>% add_rschedule(daily()) %>% add_rschedule(yearly())
    Message
      <runion[2]>
       <rrule>
       * frequency: daily
       * since: 1900-01-01
       * until: 2100-01-01
       <rrule>
       * frequency: yearly
       * since: 1900-01-01
       * until: 2100-01-01

