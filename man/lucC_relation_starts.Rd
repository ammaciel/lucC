% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lucC_allen_relations.r
\docType{data}
\name{lucC_relation_starts}
\alias{lucC_relation_starts}
\title{Allen Relation Starts}
\usage{
lucC_relation_starts(first_interval , second_interval)
}
\arguments{
\item{first_interval}{lucC_interval. An interval between two dates.}

\item{second_interval}{lucC_interval. An interval between two dates.}
}
\value{
Logical value if interval are TRUE or FALSE
}
\description{
Provide an Allen's interval relation to classified time series data. And return a logical value if an interval is TRUE or FALSE
}
\examples{
\dontrun{

library(lucC)

lucC_starting_point()

# create some examples of intervals
time5 <- lucC_interval("2011-08-01","2011-09-01")
time7 <- lucC_interval("2011-08-01","2011-09-15")

# Apply a relation 'starts' (start_I == start_J) & (end_I < end_J)
lucC_relation_starts(time5,time7)

}

}
\author{
Adeline M. Maciel
}
\keyword{datasets}
