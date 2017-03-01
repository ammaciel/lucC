#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to save raster in GeoTIFF format                 ##
##                                                             ##  
##                                             2017-02-28      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Save tibble as Raster in Folder
#' @name stilf_toGeoTIFF
#' @aliases stilf_toGeoTIFF
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot map ggplot2 for all events discovered in input data
#' 
#' @usage stilf_toGeoTIFF (data_tb = NULL, path_raster_folder = NULL)
#' 
#' @param data_tb             Tibble. A tibble with values longitude and latitude and other values
#' @param path_raster_folder  Character. Name a path folder to save raster images data
#' 
#' @keywords datasets
#' @return Images in geotiff format to open using SIG
#' @import dplyr sp raster rasterVis lattice
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # open a JSON file example
#' file_json = "./inst/example_json_Sinop_part.json"
#' 
#' input_tb_raw_json <- file_json %>% 
#'   stilf_fromJSON() 
#' input_tb_raw_json
#' 
#' # save rasters in folder
#' stilf_toGeoTIFF (input_tb_raw_json, "~/Desktop/raster")
#' 
#'  
#'}
#'

# plot maps with events
stilf_toGeoTIFF <- function(data_tb = NULL, path_raster_folder = NULL){ 
  
  if (!is.null(data_tb)) {
    mapRaster <- data_tb
  } else {
    stop("\nFile must be defined!\nThis data can be obtained using stilf predicates holds or occurs\n")
  }
  
  if (!is.null(path_raster_folder)) {
    raster_folder <- path_raster_folder
  } else {
    stop("\nEnter a path to SAVE your GeoTIFF images!\n")
  }
  
  rownames(mapRaster) <- NULL
  mapRaster <- data.frame(mapRaster) %>% dplyr::filter(mapRaster$label != "NA")
  mapRaster$label <- as.factor(mapRaster$label)
  
  # start here
  dates <- unique(format(as.Date(mapRaster$end_date), format = '%Y'))
  indexLong <- which(colnames(mapRaster) == "longitude")
  indexLat <- which(colnames(mapRaster) == "latitude")
  indexLabel <- which(colnames(mapRaster) == "label")
  
  for(i in 1:length(dates)){
    
    map <- dplyr::filter(mapRaster, grepl(dates[i], as.character(mapRaster$end_date), fixed = TRUE))
    pts <- map[c(indexLong:indexLat,indexLabel)] # long, lat and class
    colnames(pts) <- c('x', 'y', 'z')
    
    # Convert the data frame to a SpatialPointsDataFrame
    sp::coordinates(pts) = ~x+y
    
    # Convert system by CRS, and then spTransform to the destination
    sp::proj4string(pts) = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long
    pts = sp::spTransform(pts,sp::CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")) # sinusoidal
    
    # Tell R that gridded
    sp::gridded(pts) = TRUE
    
    # Raster package to convert to a raster and set its CRS
    # All data SciDB is a sinusoidal projection
    r = raster::raster(pts)
    raster::projection(r) = sp::CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
    
    my_col = rainbow(length(levels(factor(mapRaster$label))), alpha = 0.7)
    
    plot <- rasterVis::levelplot(r, col.regions = my_col, xlab=list(label="", cex=1.8), 
                      ylab=list(label="", cex=1.8), colorkey = list(space="right"), 
                      panel=lattice::panel.levelplot.raster, par.settings = list(axis.line = list(col = 0)), 
                      scales = list(draw = TRUE), 
                      main=list(paste("Raster ", dates[i], sep = ""), side=1, line=0.5),  
                      line.col= "transparent") 
    
    print(plot)
    
   # write it as a geoTIFF file using the raster package
   raster::writeRaster(r,paste(raster_folder,"/raster_",dates[i],".tif", sep = ""))
  }
  
  cat("\nGeoTIFF images saved successfully in directory: '", raster_folder, "'\n")
  
}

  
  
  