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
##                                             2017-04-18      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Apply TWDTW Classifier on Time Series Tibble (using the sits package) 
#' @name stilf_TWDTW
#' @aliases stilf_TWDTW
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Classify a set of time series from sits_table, using 
#' sits_getdata directly in the format stilf (longitude, latitude, start_date, end_date, label, id, index)
#' 
#' @usage stilf_TWDTW (data_tb = NULL, 
#' patterns_tb = NULL, bands = NULL)
#' 
#' @param data_tb       Tibble. A tibble with values from sits format
#' @param patterns_tb   Tibble. Temporal patterns in sits format. See more in sits package
#' @param bands         Character. Set of variables with values of spectral bands
#' 
#' @seealso sits details at https://github.com/gilbertocamara/sits/ package
#' 
#' @keywords datasets
#' @return Tibble with set of points  classified by TWDTW method
#' @import tibble 
#' @importFrom dplyr filter  
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom lubridate year
#' @importFrom sits sits_TWDTW
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' library(sits)
#' 
#' stilf_starting_point()
#' 
#' # open data with time series set
#' data("example_ts")
#' example_ts
#' 
#' # read a pattern table from a JSON file
#' patterns.tb <- sits_getdata("./inst/extdata/patterns/example_temporal_pattern.json")
#' patterns.tb
#' 
#' # plot patterns
#' sits_plot(patterns.tb, type = "patterns")
#' 
#' # only this bands have in patterns
#' bands <- c("ndvi", "evi", "nir")
#' 
#' # classification using stilf_TWDTW with default parameters from sits
#' example_stilf_TWDTW <- stilf_TWDTW(example_ts, patterns.tb, bands)
#' example_stilf_TWDTW
#' 
#' # plot maps
#' stilf_plot_maps_input(example_stilf_TWDTW, EPSG_WGS84 = TRUE, 
#' custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 
#' 
#'}
#'

# function to classify a set of points using TWDTW from sits package
stilf_TWDTW <- function(data_tb = NULL, patterns_tb = NULL, bands = NULL){
  
  stilf_starting_point()
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), err_desc = "data_tb tibble, must be defined!\n")
  ensurer::ensure_that(patterns_tb, !is.null(patterns_tb), err_desc = "Patterns file must be defined!")
  ensurer::ensure_that(bands, !is.null(bands), err_desc = "Bands must be defined!")
  
  # create a tiblle with classification result
  res_classification <- tibble::tibble(longitude  = double(), 
                                       latitude   = double(), 
                                       start_date = as.Date(as.character()), 
                                       end_date   = as.Date(as.character()), 
                                       label      = character(),
                                       id         = numeric(),
                                       index      = numeric())
  
  # create progress bar
  progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data_tb), style = 3)
  
  # classify input data using TWDTW from sits
  for(i in 1:nrow(data_tb)){
    
    aux <- data_tb[i,]
    
    #classify point
    matches <- sits::sits_TWDTW(aux, patterns_tb, bands = bands)
    
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
                                            start_date = as.Date(res_dist$from), 
                                            end_date   = as.Date(res_dist$to), 
                                            label      = res_dist$label,
                                            id         = 0,
                                            index      = i)  
      res_classification
      
    }
    
    # update progress bar
    utils::setTxtProgressBar(progress_bar, i)
  }
  
  close(progress_bar)
  
  # remove data with 2000 in end_data
  res_classification <- dplyr::filter(res_classification, 
                                      !grepl("2000", as.character(res_classification$end_date), 
                                             fixed = TRUE))
  
  # order id by number rows
  res_classification$id <- 1:nrow(res_classification)
  
  return(res_classification)

}


#' @title Convert result of sits_TWDTW tibble to stilf format
#' @name stilf_TWDTW_fromSITS
#' @aliases stilf_TWDTW_fromSITS
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Convert a tibble classified using sits_TWDTW 
#' to stilf format (longitude, latitude, start_date, end_date, label, id, index)
#' 
#' @usage stilf_TWDTW_fromSITS (data_tb = NULL)
#' 
#' @param data_tb       Tibble. A tibble with values in sits format
#' 
#' @seealso sits details at https://github.com/gilbertocamara/sits/
#' package
#' 
#' @keywords datasets
#' @return Tibble with set of points classified by TWDTW method
#' @import tibble 
#' @importFrom dplyr filter  
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom lubridate year
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' library(sits)
#' 
#' stilf_starting_point()
#' 
#' # open data with time series set
#' data("example_ts")
#' example_ts
#' 
#' # read a pattern table from a JSON file
#' patterns.tb <- sits_getdata("./inst/extdata/patterns/example_temporal_pattern.json")
#' patterns.tb
#' 
#' # plot patterns
#' sits_plot(patterns.tb, type = "patterns")
#' 
#' # only this bands have in patterns
#' bands <- c("ndvi", "evi", "nir")
#' 
#' # classification using sits_TWDTW (using sits package)
#' example_sits_TWDTW <- sits_TWDTW(example_ts, patterns.tb, bands)
#' example_sits_TWDTW
#' 
#' # convert example_sits_TWDTW from sits package to stilf format
#' example_convert_sits_TWDTW <- stilf_TWDTW_fromSITS(data_tb = example_sits_TWDTW)
#' example_convert_sits_TWDTW
#' 
#' # plot maps
#' stilf_plot_maps_input(example_convert_sits_TWDTW, EPSG_WGS84 = TRUE, 
#' custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 
#' 
#' 
#'}
#'

# function to classify a set of points using TWDTW from sits package
stilf_TWDTW_fromSITS <- function(data_tb = NULL){
  
  stilf_starting_point()
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), err_desc = "data_tb tibble from sits_TWDTW, must be defined!\n")
  
  # create a tiblle with classification result
  res_classification <- tibble::tibble(longitude  = double(), 
                                       latitude   = double(), 
                                       start_date = as.Date(as.character()), 
                                       end_date   = as.Date(as.character()), 
                                       label      = character(),
                                       id         = numeric(),
                                       index      = numeric())
  
  # create progress bar
  progress_bar <- utils::txtProgressBar(min = 0, max = nrow(data_tb), style = 3)
  
  # classify input data using TWDTW from sits
  for(i in 1:nrow(data_tb)){
    
    aux <- data_tb[i,]
    
    #classify point
    matches <- aux
    
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
                                            start_date = as.Date(res_dist$from), 
                                            end_date   = as.Date(res_dist$to), 
                                            label      = res_dist$label,
                                            id         = 0,
                                            index      = i) 
      res_classification
      
    }
    
    # update progress bar
    utils::setTxtProgressBar(progress_bar, i)
  }
  
  close(progress_bar)
  
  # remove data with 2000 in end_data
  res_classification <- dplyr::filter(res_classification, 
                                      !grepl("2000", as.character(res_classification$end_date), 
                                             fixed = TRUE))
  
  # order id by number rows
  res_classification$id <- 1:nrow(res_classification)
  
  return(res_classification)
  
}
  