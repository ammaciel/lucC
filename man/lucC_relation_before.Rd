% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lucC_allen_relations.r
\docType{data}
\name{lucC_relation_before}
\alias{lucC_relation_before}
\title{Allen Relation Before}
\usage{
lucC_relation_before(first_interval , second_interval)
}
\arguments{
\item{first_interval}{lucC_interval. An interval between two dates.}

\item{second_interval}{lucC_interval. An interval between two dates.}
}
\value{
Logical value if interval are TRUE or FALSE
}
\description{
Provide an Allen's interval relation to classified time series data. 
And return a logical value if an interval is TRUE or FALSE
}
\examples{
\dontrun{

library(lucC)

lucC_starting_point()

# create some examples of intervals
time1 <- lucC_interval("2011-09-01","2011-10-01")
time4 <- lucC_interval("2011-10-01","2011-11-01")

# Two interval I and J, with:
# I <- ("2011-08-15","2011-08-29")
# J <- ("2011-09-01","2011-10-01")
# end_I equals to the final value of the interval, I == "2011-08-29"
# start_I equals to the begin value of the interval, I == "2011-08-15"
# end_J equals to the final value of the interval, J == "2011-10-01"
# start_J equals to the begin value of the interval, J == "2011-09-01"

# Apply a relation 'before' (end_I < start_J)
lucC_relation_before(time1,time4)

           
}

}
\author{
Adeline M. Maciel
}
\keyword{datasets}
