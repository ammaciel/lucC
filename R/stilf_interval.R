#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with standard format to interval                 ##
##                                                             ##
##                                             2017-02-26      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title STILF Intervals
#' @name stilf_interval
#' @aliases stilf_interval
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a stilf_interval, an input pattern format used by Allen's relations. And return a value of interval
#'
#' @usage stilf_interval(first_date , second_date)
#' 
#' @param first_date      Date. An date value like '2012-03-02'.
#' @param second_date     Date. An date value like '2013-03-02'.
#' 
#' @keywords datasets
#' @return Interval value with two dates 
#' @import lubridate dplyr
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # create some examples of intervals
#' time1 <- stilf_interval("2011-09-01","2011-10-01")
#' time2 <- stilf_interval("2011-09-15","2011-11-01")
#'
#'
#'}
#'
#'

# Transform two dates in an interval
stilf_interval <- function (first_date, second_date) {
  
  if (!is.null(first_date) & !is.null(second_date) ) {
    fir_date <- first_date
    sec_date <- second_date
  } else {
    cat("\nDates must be defined! \n")
  }
    
    # test if a valid date
    pattern = c('[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]')
    
    if (!is.null(grepl(pattern, fir_date) == TRUE) & !is.null(grepl(pattern, sec_date) == TRUE)){
       
      lubridate::interval(lubridate::ymd(fir_date), lubridate::ymd(sec_date))
      
    } else {
      cat("\nEnter with a date in format 'year-month-day' = '2010-01-02'\n")
    }
    
}


