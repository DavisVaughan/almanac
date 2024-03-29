% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcustom.R
\name{rcustom}
\alias{rcustom}
\title{Create a custom rschedule}
\usage{
rcustom(events)
}
\arguments{
\item{events}{\verb{[Date]}

A vector of event dates.}
}
\value{
A custom rschedule.
}
\description{
\code{rcustom()} creates an rschedule from manually defined event dates. This can
be useful when combined with \code{\link[=runion]{runion()}} and \code{\link[=rsetdiff]{rsetdiff()}} if you have a set of
fixed event dates to forcibly include or exclude from an rschedule.
}
\examples{
include <- rcustom("2019-07-05")
exclude <- rcustom("2019-07-04")

independence_day <- yearly() \%>\%
  recur_on_month_of_year("July") \%>\%
  recur_on_day_of_month(4)

# Remove forcibly excluded day
independence_day <- rsetdiff(independence_day, exclude)

# Add forcibly included day
independence_day <- runion(independence_day, include)

alma_search("2018-01-01", "2020-12-31", independence_day)
}
