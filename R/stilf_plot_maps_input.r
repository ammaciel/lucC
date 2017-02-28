#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to plot input data                               ##
##                                                             ##  
##                                             2017-02-26      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Plot Input Maps 
#' @name stilf_plot_maps_input
#' @aliases stilf_plot_maps_input
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot map ggplot2 for all input data
#' 
#' @usage stilf_plot_maps_input (data_tb = NULL, EPSG_WGS84 = FALSE)
#' 
#' @param data_tb       Tibble. A tibble with values longitude and latitude and other values
#' @param EPSG_WGS84    Character. A reference coordinate system. If TRUE, the values of latitude and longitude alredy use this coordinate system, if FALSE, the data set need to be transformed

#' @keywords datasets
#' @return Plot with input data as colored map
#' @import dplyr sp ggplot2 
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # open a CSV file example
#' file_json = "./inst/example_json_Sinop_part.json"
#' 
#' # open file JSON
#' input_tb_raw_json <- file_json %>% 
#'   stilf_fromJSON() 
#' input_tb_raw_json
#' 
#' # plot maps input data
#' stilf_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)
#' 
#' 
#'}
#'

# plot maps for input data
stilf_plot_maps_input <- function(data_tb = NULL, EPSG_WGS84 = FALSE){ 
  
  if (!is.null(data_tb)) {
    input_data <- data_tb
  } else {
    stop("\nFile must be defined!\n")
  }
  
  # create points  
  .createPoints(input_data, EPSG_WGS84)
  
  a <- data.frame(Reduce(rbind, points_input_map.list))
  
  rownames(a) <- NULL
  a <- data.frame(a) %>% dplyr::filter(a$w != "NA")
  a$x <- as.integer(a$x)
  a$y <- as.integer(a$y)
  a$w <- as.factor(a$w)
  a$z <- as.factor(a$z)
  map_input_df <- NULL
  map_input_df <- a
  
  # plot images all years
  g <- ggplot2::ggplot(map_input_df, aes(map_input_df$x, map_input_df$y)) +
        geom_raster(aes_string(fill=map_input_df$"z")) +
        scale_y_continuous(expand = c(0, 0), breaks = NULL) +
        scale_x_continuous(expand = c(0, 0), breaks = NULL) +
        facet_wrap("w") +
        coord_fixed(ratio = 1) + 
        #coord_fixed(ratio = 1/cos(mean(x)*pi/180)) +
        theme(legend.position = "bottom", strip.text = element_text(size=10)) +
        xlab("") +
        ylab("") +
        scale_fill_brewer(name="Legend:", palette= "Set3")

  print(g)
  
  map_input_df <<- map_input_df
  
}


# create points
.createPoints <- function(input_data, EPSG_WGS84){ 
  
  map_tb <- input_data 

  # starts here
  dates <- unique(format(as.Date(map_tb$end_date), format = '%Y'))
  indexLong <- which(colnames(map_tb) == "longitude")
  indexLat <- which(colnames(map_tb) == "latitude")
  indexLabel <- which(colnames(map_tb) == "label")
  
  # save points in environment
  points_input_map.list <- NULL
  points_input_map.list <<- list()
  
  for(x in 1:length(dates)){
 
    map <- dplyr::filter(map_tb, grepl(dates[x], as.character(map_tb$end_date), fixed = TRUE))
    pts <- map[c(indexLong:indexLat,indexLabel)] # long, lat and class
    colnames(pts) <- c('x', 'y', 'z')
    
    if (EPSG_WGS84 == TRUE) {
      # converte to sinusoidal projection in case values in Longitude and Latitude
      d <- data.frame("x" = pts$x, "y" = pts$y, "z" = pts$z, "w"= dates[x])
      sp::coordinates(d) <- cbind(pts$x, pts$y)  
      sp::proj4string(d) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      CRS.new <- sp::CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
      d <- sp::spTransform(d, CRS.new)
      
    } else if (EPSG_WGS84 == FALSE) {
      # use in case data from SciDB col and row
      d <- data.frame(x=pts$x, y=pts$y, z=pts$z, w=dates[x])
      sp::coordinates(d) <- cbind(pts$x, pts$y)  
    } else {
      stop("FALSE/TRUE")
    }
    
    pts1 <- as.data.frame(d)
    colnames(pts1) <- c('x1', 'y1', 'z', 'w', 'x', 'y')
    pts1 <- data.frame(pts1$x,pts1$y,pts1$z,pts1$w,pts1$x1,pts1$y1)
    names(pts1)[1:6] = c('x', 'y', 'z','w','x1', 'y1')
    points_input_map.list[[paste("pts_",dates[x], sep = "")]] <<- pts1

  }
  
}  




