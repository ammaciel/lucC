#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with predicates holds(o,p,t) and occur(o,p,Te)   ##
##                                                             ##  
##                                             2017-06-23      ##
##                                                             ##
##  J. F. Allen.  end_datewards a general theory of action and ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


#' @title Predicate Allen Holds
#' @name stilf_predicate_holds
#' @aliases stilf_predicate_holds
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a predicate of Allen's which asserts that a property holds 
#' during a time interval. Return a tibble with value within defined interval 
#' 
#' @usage stilf_predicate_holds (locations = NULL, location_properties = NULL, 
#' time_intervals = stilf_interval("2000-01-01", "2004-01-01"))
#' 
#' @param locations           Tibble. A tibble with values longitude and latitude and other values
#' @param location_properties Character. Name of value present in a row of the tibble, such as 'Forest' or other value
#' @param time_intervals      Interval. A interval of time to verify if location_properties is over or 
#' not in stilf_interval format. Given a tibble with values, will be asserts if that location_properties 
#' of locations holds during a time interval. 
#' 
#' @keywords datasets
#' @return Tibble with all events hold during a time interval
#' @importFrom lubridate int_standardize int_start int_end
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' json_file = "./inst/extdata/patterns/example_TWDTW.json"
#' 
#' input_tb_json <- json_file %>% 
#'   stilf_fromJSON()  
#' input_tb_json
#' 
#' # example of application
#' time_ex1 <- stilf_interval("2001-01-01", "2003-01-01")
#' time_ex1
#' time_ex2 <- stilf_interval("2005-01-01", "2010-01-01")
#' time_ex2
#' 
#' # location_properties
#' properties <- "Forest"
#' 
#' # example predicate holds
#' stilf_predicate_holds(locations = input_tb_json, 
#' location_properties = "Forest", time_intervals = time_ex1)
#' 
#' stilf_predicate_holds(locations = input_tb_json, 
#' location_properties = properties, time_intervals = time_ex2)
#' 
#'}
#'
#'

# HOLDS(property, time) 
# Asserts that a property holds during a time interval
# version: 1 
# format: holds(o,p,t)
# parameters: o = geo-objects, p = properties of objects and t = time intervals

stilf_predicate_holds <- function(locations = NULL, location_properties = NULL, time_intervals = stilf_interval("2000-01-01", "2004-01-01")){
 
  if (!is.null(locations) & !is.null(location_properties) & !is.null(time_intervals)) {
    o <- locations
    p <- location_properties
    t <- lubridate::int_standardize(time_intervals)
  } else {
    stop("\nParameters:\n locations (data_df),\n 
         location_properties ('Forest') and\n 
         time_intervals (stilf_interval('2000-01-01', '2004-01-01')),\n 
         must be defined!\n")
  }
  
  intStart <- format(lubridate::int_start(t), format = '%Y-%m-%d')
  intEnd <- format(lubridate::int_end(t), format = '%Y-%m-%d')
  
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
  
  # if(nrow(aux.df) > 0){
  #   cat("\nHave been found ", nrow(aux.df)," properties which holds during a time interval.\n")
  # } else {
  #   cat("\nAny property have been founded. Alter your location_properties or time_intervals parameters.\n")
  # }
  # 
  return(aux.df)
}


#' #' @title Predicate Allen Occur
#' #' @name stilf_predicate_occur
#' #' @aliases stilf_predicate_occur
#' #' @author Adeline M. Maciel
#' #' @docType data
#' #'
#' #' @description Provide a predicate of Allen's which asserts that an event happened over a time interval
#' #'
#' #' @usage stilf_predicate_occur (locations = NULL, location_properties = NULL,
#' #' event_time_intervals = stilf_interval("2000-01-01", "2004-01-01"))
#' #'
#' #' @param locations           Tibble. A tibble with values longitude and latitude and other values
#' #' @param location_properties     Character. Name of value present in a row of the tibble, such as 'Forest' or other value
#' #' @param event_time_intervals  Interval. A interval of time to verify if event about location_properties occurs or not in stilf_interval format
#' #'
#' #' @keywords datasets
#' #' @return Tibble with all events happened over a time interval
#' #' @importFrom lubridate int_standardize int_start int_end
#' #' @importFrom dplyr bind_rows
#' 
#' #' @export
#' #'
#' #' @examples \dontrun{
#' #'
#' #' library(stilf)
#' #'
#' #' stilf_starting_point()
#' #'
#' #' file_json = "./inst/extdata/patterns/example_TWDTW.json"
#' #' input_tb_raw_json <- file_json %>%
#' #'   stilf_fromJSON()
#' #' input_tb_raw_json
#' #'
#' #' # plot maps input data
#' #' stilf_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)
#' #'
#' #' # define interval
#' #' time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")
#' #' time_ex1
#' #'
#' #' # using occur
#' #' ts_occur1 <- stilf_predicate_occur(locations = input_tb_raw_json,
#' #' location_properties = "Pasture", event_time_intervals = time_ex1)
#' #' ts_occur1
#' #'
#' #' ts_occur2 <- stilf_predicate_occur(locations = input_tb_raw_json,
#' #' location_properties = "Forest", event_time_intervals = time_ex1)
#' #' ts_occur2
#' #'
#' #' # events over input map
#' #' stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE)
#' #' stilf_plot_maps_events(ts_occur2, EPSG_WGS84 = TRUE)
#' #'
#' #'}
#' #'
#' 
#' # OCCUR(event, time)
#' # Asserts that an event happened over a time interval
#' # version: 2
#' # format: occur(o,p,Te)
#' # parameters: o = geo-objects, p = properties of objects and Te = event time intervals
#' 
#' stilf_predicate_occur <- function(locations = NULL, location_properties = NULL, event_time_intervals = stilf_interval("2000-01-01", "2004-01-01")){
#' 
#'   if (!is.null(locations) & !is.null(location_properties) & !is.null(event_time_intervals)) {
#'     o <- locations
#'     p <- location_properties
#'     te <- lubridate::int_standardize(event_time_intervals)
#'   } else {
#'     stop("\nParameters:\n locations (data_df),\n
#'          location_properties ('Forest') and\n
#'          event_time_intervals (stilf_interval('2000-01-01', '2004-01-01')),\n
#'          must be defined!\n")
#'   }
#' 
#'   intStart <- format(lubridate::int_start(te), format = '%Y-%m-%d')
#'   intEnd <- format(lubridate::int_end(te), format = '%Y-%m-%d')
#' 
#'   df <- o
#'   df <- df[order(df$end_date),] # order by end_date
#'   p <- as.character(p)
#'   aux.df = df[FALSE,]
#' 
#'   for (i in 1:nrow(df)) {
#' 
#'     if ((df$label[i] == p) & ((df$start_date[i] >= intStart) & (df$end_date[i] <= intEnd))) {
#'       aux.df <- dplyr::bind_rows(aux.df,df[i,])
#'       #cat(sprintf("time: %d in interval: %s -- %s = TRUE \n", i, df$start_date[i], df$end_date[i]))
#'     } else {
#'       # cat(sprintf("time: %d in interval: %s -- %s = FALSE \n", i, df$start_date[i], df$start_date[i]))
#'     }
#'   }
#' 
#' #   if(nrow(aux.df) > 0){
#' #     cat("\nHave been found ", nrow(aux.df)," events which happened over a time interval.\n")
#' #   } else {
#' #     cat("\nAny event have been founded. Alter your location_properties or event_time_intervals parameters.\n")
#' #   }
#' 
#'   return(aux.df)
#' }
#' 

