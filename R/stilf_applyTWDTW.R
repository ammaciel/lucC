#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script classify time series using TWDTW                 ##
##                                                             ##  
##                                             2017-02-28      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Stilf Apply TWDTW from SITS Format
#' @name stilf_applyTWDTW
#' @aliases stilf_applyTWDTW
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Classify a set of time series in sits format to stilf format
#' 
#' @usage stilf_applyTWDTW (data_tb = NULL, 
#' patterns_tb = NULL, bands = NULL)
#' 
#' @param data_tb       Tibble. A tibble with values in sits format
#' @param patterns_tb   Tibble. Temporal patterns in sits format. See more in sits 
#' package
#' @param bands         Character. Set of variables with values of spectral bands
#' 
#' @seealso sits details at https://github.com/gilbertocamara/sits/
#' package
#' 
#' @keywords datasets
#' @return Tibble with set of points  classified by TWDTW method
#' @import dplyr sits tibble 
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' library(sits)
#' 
#' zip_point_tb <- unzip("./inst/list_time_series_Area1_Sinop.zip")
#' point_tb <- sits::sits_getdata(zip_point_tb)
#' 
#' ts_data <- point_tb
#' 
#' # read a pattern table from a JSON file
#' patterns_tb <- sits_getdata("./inst/patterns/patterns_
#' Damien_Ieda_Rodrigo_14classes_3bands_Rename_Labels_Sep.json")
#' 
#' # only this bands have in patterns
#' bands <- c("ndvi", "evi", "nir")
#' 
#' # plot patterns
#' sits_plot(patterns_tb, type = "patterns")
#' 
#' # json have other attributes
#' values <- c("Index", "ndvi", "evi", "nir") # 
#' 
#' # remove other attributes
#' ts_data$time_series <- lapply(ts_data$time_series,function(p){
#'   p <- p[,values, drop = FALSE]
#' })
#' 
#' # classify with TWDTW and return a format to stilf
#' res <- stilf_applyTWDTW(ts_data, patterns_tb, bands)
#' 
#' res
#' 
#' 
#'}
#'

# function to classify a set of points using TWDTW from sits package
stilf_applyTWDTW <- function(data_tb = NULL, patterns_tb = NULL, bands = NULL){
  
  stilf_starting_point()
  
  if (!is.null(data_tb)) {
    input_data <- data_tb
  } else {
    stop("\nFile must be defined!\n")
  }
  
  # create a tiblle with classification result
  res_classification <- tibble::tibble(longitude  = double(), 
                                       latitude   = double(), 
                                       start_date = character(), 
                                       end_date   = character(), 
                                       label      = character(),
                                       id         = numeric(),
                                       index      = numeric(),
                                       twdtw_dist = double())
  
  # create progress bar
  progress_bar <- txtProgressBar(min = 0, max = nrow(input_data), style = 3)
  
  # classify input data using TWDTW from sits
  for(i in 1:nrow(input_data)){
    
    Sys.sleep(0.0)
    
    aux <- input_data[i,]
    
    #classify point
    matches <- sits::sits_TWDTW(aux, patterns_tb, bands)
    
    # plot the classification
    # print(plot(x = matches, type = "classification", overlap = 0.5))
    
    dates_to <- unique(lubridate::year(matches$matches[[1]][[1]]$to)) # new structure from sits
    
    temp <- matches$matches[[1]][[1]]
    
    # get the  minimum distance for classification for each year
    for(x in 1:length(dates_to)){
      
      res_to <- dplyr::filter(temp, grepl(dates_to[x], as.character(temp$to), fixed = TRUE))
      
      res_dist <- res_to[which.min(res_to$distance), ]
      
      # save value in tibble
      res_classification <- tibble::add_row(res_classification, 
                                            longitude  = aux$longitude, 
                                            latitude   = aux$latitude, 
                                            start_date = res_dist$from, 
                                            end_date   = res_dist$to, 
                                            label      = res_dist$label,
                                            id         = 0,
                                            index      = i,
                                            twdtw_dist = res_dist$distance) 
      res_classification
      
    }
    
    # update progress bar
    setTxtProgressBar(progress_bar, i)
  }
  
  #close(progress_bar)
  
  # remove data with 2000 in end_data
  res_classification <- dplyr::filter(res_classification, 
                                      !grepl("2000", as.character(res_classification$end_date), 
                                             fixed = TRUE))
  
  # order id by number rows
  res_classification$id <- 1:nrow(res_classification)
  
  return(res_classification)

}


  