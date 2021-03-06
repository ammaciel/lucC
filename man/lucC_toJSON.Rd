% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lucC_load_data.R
\docType{data}
\name{lucC_toJSON}
\alias{lucC_toJSON}
\title{Save result as JSON format}
\usage{
lucC_toJSON (data_tb, path_json_file = NULL)
}
\arguments{
\item{data_tb}{Tibble. A data frame with input values}

\item{path_json_file}{Character. Name path and file to save JSON file data}
}
\value{
JSON format to file stored
}
\description{
Save the output data in JSON format in an user's directory
}
\examples{
\dontrun{

library(lucC)

lucC_starting_point()

file_csv = "./data/example_TWDTW.csv"

input_tb_csv <- file_csv \%>\% 
  lucC_fromCSV(separator = ",", header_file = TRUE) #\%>\% 
input_tb_csv

# save the input as json format
output_file = "~/Desktop/example_json.json"
lucC_toJSON(input_tb_csv, output_file)

}

}
\author{
Adeline M. Maciel
}
\keyword{datasets}
