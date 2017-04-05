
#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to define a stardard date to extract events      ##
##                                                             ##  
##                                             2017-03-01      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Stilf Apply TWDTW from SITS Format
#' @name stilf_standard_date_events
#' @aliases stilf_standard_date_events
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Create two new columns in tibble with standard date in 
#' order to extract events
#' 
#' @usage stilf_standard_date_events(data_tb = NULL, 
#' month_year = "09", day_month = "01")
#' 
#' @param data_tb      Tibble. A tibble with values in sits format
#' @param month_year   Character. A month of the year in two digits (01 to 12)
#' @param day_month    Character. A day of the month in two digits (01 to 31). 
#' Depends the character calendar month. 
#' 
#' @keywords datasets
#' @return Tibble containing two new columns with start_date and end_date in a 
#' predefined standard range ensurer
#' 
#' @import tibble 
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()

#' file_json_zip = "./inst/areaSinop/classification_14patterns_3bands_Area1_Sinop.zip"

#' input_tb_area_Sinop <- file_json_zip %>% 
#'   unzip() %>% 
#'   stilf_fromJSON() 
#' 
#' input_tb_area_Sinop
#' 
#' #remove columns and pass to stilf format
#' new_area_Sinop <- input_tb_area_Sinop %>% 
#'   select(longitude,latitude,label,start_date,end_date,id,index) %>% 
#'   stilf_data_preparation()
#' 
#' new_area_Sinop
#' 
#' stilf_standard_date_events(data_tb = df, month_year = "09", day_month = "01")
#' 
#'}
#'

stilf_standard_date_events <- function(data_tb = NULL, month_year = "09", day_month = "01"){
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, file must be defined!")
  ensurer::ensure_that(month_year, !is.null(month_year), 
                       err_desc = "month_year must be defined! Default is '08', August.")
  ensurer::ensure_that(day_month, !is.null(day_month), 
                       err_desc = "day_month date must be defined! Default is day '15'.")
  
  input_data <- data_tb 
  
  # create new columns with an uniform date
  input_data$"start_date2" <- as.Date(input_data$start_date, format = '%Y-%m-%d')   
  input_data$start_date <- as.Date(paste0(lubridate::year(input_data$start_date),"-", month_year,"-", day_month, sep = ""), format = '%Y-%m-%d') 
  
  input_data$"end_date2" <- as.Date(input_data$end_date, format = '%Y-%m-%d')
  input_data$end_date <- as.Date(paste0(lubridate::year(input_data$end_date),"-", month_year,"-", day_month, sep = ""), format = '%Y-%m-%d') 
  
  return(input_data)
  
}

