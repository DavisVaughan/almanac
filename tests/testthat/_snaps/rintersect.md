# rintersect() generates informative output

    Code
      # # Empty rintersect
      rintersect()
    Message
      <rintersect[0]>
    Code
      # # With rschedules
      rintersect() %>% add_rschedule(daily()) %>% add_rschedule(yearly())
    Message
      <rintersect[2]>
       <rrule>
       * frequency: daily
       * since: 1900-01-01
       * until: 2100-01-01
       <rrule>
       * frequency: yearly
       * since: 1900-01-01
       * until: 2100-01-01

