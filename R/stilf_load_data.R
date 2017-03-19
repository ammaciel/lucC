#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with format as tibble the input data             ##
##                                                             ##
##                                             2017-02-26      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Save result as JSON format
#' @name stilf_toJSON
#' @aliases stilf_toJSON
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Save the output data in JSON format in an user's directory 
#' @usage stilf_toJSON (data_tb, path_json_file = NULL)
#' 
#' @param data_tb        Tibble. A data frame with input values
#' @param path_json_file Character. Name path and file to save JSON file data

#' @keywords datasets
#' @return JSON format to file stored
#' @import jsonlite dplyr readr ensurer
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # Example to save data in JSON format
#' file = "./inst/example_csv_file.csv"
#' 
#' input_tb_csv <- file %>% 
#'   stilf_fromCSV(separator = ",", header_file = TRUE) 
#' input_tb_csv
#' 
#' output_file = "~/Desktop/output_file.json"
#' stilf_toJSON(input_tb_csv, output_file)
#' 
#'
#'}
#'
#'

stilf_toJSON <- function (data_tb, path_json_file = NULL) {
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, must be defined!\n")
  ensurer::ensure_that(path_json_file, !is.null(path_json_file), 
                       err_desc = "Enter a path to SAVE your data!")
  
  input_data <- as.data.frame(data_tb)
  
  input_data %>%
    jsonlite::toJSON (pretty = TRUE, digits=15) %>%
    readr::write_lines (path_json_file)
  
  cat("\nFile CSV format saved successfully!\n")
  
  return (path_json_file)
  
}


#' @title Open JSON file
#' @name stilf_fromJSON
#' @aliases stilf_fromJSON
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Open the input data in JSON format in an user's directory 
#' @usage stilf_fromJSON (path_json_file = NULL)
#' 
#' @param path_json_file Character. Name path and file to open JSON file data

#' @keywords datasets
#' @return Open JSON format to file
#' @import jsonlite dplyr ensurer
#' @export
#'
#'
#' @examples \dontrun{
#' # Open a data example
#' library(stilf)
#' 
#' json_file = "./inst/example_json_file.json"
#' 
#' # open a json data and store it format stilf_input_data
#' input_tb_json <- json_file %>% 
#'   stilf_fromJSON()  
#' input_tb_json
#' 
#'
#'}
#'
#'

stilf_fromJSON <- function (path_json_file = NULL) {
  
  # Ensure if parameters exists
  ensurer::ensure_that(path_json_file, !is.null(path_json_file), 
                       err_desc = "Enter with a path to OPEN your data!")
  
  data_tb <- path_json_file %>%
    jsonlite::fromJSON () %>% 
    tibble::as_tibble()

  data_tb
  
  cat("\nFile JSON format opened successfully!\n")
  
  return(data_tb)
  
}



#' @title Open CSV file
#' @name stilf_fromCSV
#' @aliases stilf_fromCSV
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Open the input data in CSV format in an user's directory 
#' @usage stilf_fromCSV (path_csv_file = NULL, separator = ",", header_file = TRUE)
#' 
#' @param path_csv_file Character. Name path and file to open CSV file data
#' @param separator     Character. Separator to csv, i.e. ',' or ';' ...
#' @param header_file   Character. Header file csv, if TRUE or FALSE, but in this application is necessary have

#' @keywords datasets
#' @return Open a CSV format to file
#' @import jsonlite dplyr ensurer
#' @export
#'
#' @examples \dontrun{
#' # Open a data example
#' library(stilf)
#' 
#' file_csv = "./inst/example_csv_file.csv"
#' 
#' # read a csv into stilf_input_data format
#' input_tb_csv <- file_csv %>% 
#'   stilf_fromCSV(separator = ",", header_file = TRUE) 
#' input_tb_csv
#'
#'}
#'
#'

stilf_fromCSV <- function (path_csv_file = NULL, separator = ",", header_file = TRUE) {
  
  # Ensure if parameters exists
  ensurer::ensure_that(path_csv_file, !is.null(path_csv_file), 
                       err_desc = "Enter with a path to OPEN your data!")
  ensurer::ensure_that(separator, !is.null(separator), 
                       err_desc = "Define type of separator from CSV file! Default id ','")
  ensurer::ensure_that(header_file, !is.null(header_file), 
                       err_desc = "Define if file has or not a header! Default is TRUE")
  
  data_tb <- path_csv_file %>%
    read.csv (sep = separator, header = header_file, stringsAsFactors = FALSE) %>% 
    tibble::as_tibble()
  
  data_tb
  
  cat("\nFile CSV format opened successfully!\n")
  
  return(data_tb)
  
}




#' @title Example CSV File
#' @name example_csv_file
#'
#' @description Dataset of example with input data in format csv to run some functions this package.With data from a particular region from Sinop municipality, Mato Grosso, Brazil.
NULL
