#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with predicate occur(o,p,Te) a complete tibble   ##
##                                                             ##  
##                                             2017-03-01      ##
##                                                             ##
##  J. F. Allen.  end_datewards a general theory of action and ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


#' @title Predicate Allen Occur Big Data
#' @name stilf_predicate_occur_BigData
#' @aliases stilf_predicate_occur_BigData
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a predicate of Allen's which asserts that an event 
#' happened over a time interval. This version don't show a text progress 
#' bar or other message
#' 
#' @usage stilf_predicate_occur_BigData (geo_objects = NULL, object_properties = NULL, 
#' event_time_intervals = stilf_interval("2000-01-01", "2004-01-01"))
#' 
#' @param geo_objects           Tibble. A tibble with values longitude and latitude and other values
#' @param object_properties     Character. Name of value present in a row of the tibble, such as 'Forest' or other value
#' @param event_time_intervals  Interval. A interval of time to verify if 
#' event about object_properties occurs or not in stilf_interval format 
#' 
#' @keywords datasets
#' @return Tibble with all events happened over a time interval
#' @import dplyr lubridate
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#'
#' # open json file
#' json_file = "./inst/example_json_file.json"
#'
#' input_tb <- json_file %>% 
#'   stilf_fromJSON()
#'
#' input_tb
#'
#' # example of application
#' time_ex1 <- stilf_interval("2001-01-01", "2003-01-01")
#' time_ex1
#' time_ex2 <- stilf_interval("2005-01-01", "2010-01-01")
#' time_ex2
#' 
#' # object_properties
#' properties <- "Soybean_Fallow"
#' 
#' # example predicate occur
#' stilf_predicate_occur(geo_objects = new_a, 
#' object_properties = "Forest", event_time_intervals = time_ex1)
#' 
#' stilf_predicate_occur_BigData(geo_objects = input_tb, 
#' object_properties = properties, event_time_intervals = time_ex2)
#' 
#'}
#'
#'

# OCCUR(event, time) 
# Asserts that an event happened over a time interval
# version: 2 
# format: occur(o,p,Te)
# parameters: o = geo-objects, p = properties of objects and Te = event time intervals

stilf_predicate_occur_BigData <- function(geo_objects = NULL, object_properties = NULL, event_time_intervals = stilf_interval("2000-01-01", "2004-01-01")){
  
  if (!is.null(geo_objects) & !is.null(object_properties) & !is.null(event_time_intervals)) {
    o <- geo_objects
    p <- object_properties
    te <- lubridate::int_standardize(event_time_intervals)
  } else {
    stop("\nParameters:\n geo_objects (data_df),\n 
         object_properties ('Forest') and\n 
         event_time_intervals (stilf_interval('2000-01-01', '2004-01-01')),\n 
         must be defined!\n")
  }
  
  intStart <- format(lubridate::int_start(te), format = '%Y-%m-%d')
  intEnd <- format(lubridate::int_end(te), format = '%Y-%m-%d')
  
  df <- o
  df <- df[order(df$end_date),] # order by end_date
  p <- as.character(p)
  aux.df = df[FALSE,]
  
  for (i in 1:nrow(df)) {
    
    if ((df$label[i] == p) & ((df$start_date[i] >= intStart) & (df$end_date[i] <= intEnd))) {
      aux.df <- dplyr::bind_rows(aux.df,df[i,])
      #cat(sprintf("time: %d in interval: %s -- %s = TRUE \n", i, df$start_date[i], df$end_date[i]))
    } else {
      # cat(sprintf("time: %d in interval: %s -- %s = FALSE \n", i, df$start_date[i], df$start_date[i]))
    }
   
  }
  
#   if(nrow(aux.df) > 0){
#     cat("\nHave been found ", nrow(aux.df)," events which happened over a time interval.\n")
#   } else {
#     cat("\nAny event have been founded. Alter your object_properties or event_time_intervals parameters.\n")
#   }
  
  return(aux.df)
  }


