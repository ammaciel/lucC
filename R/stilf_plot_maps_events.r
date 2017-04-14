#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to plot events as maps and sequences             ##
##                                                             ##  
##                                             2017-02-26      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Plot Events over Maps with STILF
#' @name stilf_plot_maps_events
#' @aliases stilf_plot_maps_events
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot map ggplot2 for all events discovered in input data
#' 
#' @usage stilf_plot_maps_events (data_tb = NULL, EPSG_WGS84 = TRUE, 
#' custom_palette = FALSE, RGB_color = NULL, shape_point = 0, 
#' colour_point = "black" , size_point= 1)
#' 
#' @param data_tb         Tibble. A tibble with values longitude and latitude and other values
#' @param EPSG_WGS84      Character. A reference coordinate system. If TRUE, the values of latitude and longitude alredy use this coordinate system, if FALSE, the data set need to be transformed
#' @param custom_palette  Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE
#' @param RGB_color       Character. A vector with color names to map legend, for example, c("Green","Blue"). Default is the color brewer 'Paired'
#' @param shape_point     Numeric or Character. Is a shape point for events highlighted over map. Default is 0.  This includes different points symbols commonly used in R as "pch", such as numeric values like 0 to square, 1 to circle and 4 to cross shape. And also other characters can be used including ".", "+", "*", "-", "#".    
#' @param colour_point    Numeric. Is a colour for the shape point for events highlighted over map. Default is black
#' @param size_point      Numeric. Is a size of the shape point around pixels in plot. Default is 1
#' 
#' @keywords datasets
#' @return Plot with input data as colored map
#' @import ggplot2 magrittr
#' @importFrom ensurer ensure_that 
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr filter
#' @importFrom lubridate year 
#' @importFrom sp proj4string CRS spTransform coordinates
#'  
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
#' # open file JSON
#' input_tb_raw_json <- file_json %>% 
#'   stilf_fromJSON() 
#' input_tb_raw_json
#' 
#' # plot maps input data
#' stilf_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)
#' 
#' # define interval
#' time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")
#'  
#' # apply predicate occur
#' ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, 
#' object_properties = "Forest", event_time_intervals = time_ex1)
#' 
#' # events over input map
#' stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, 
#' custom_palette = FALSE, size_point = 1)
#' 
#'}
#'

# plot maps with events
stilf_plot_maps_events <- function(data_tb = NULL, EPSG_WGS84 = TRUE, custom_palette = FALSE, RGB_color = NULL, shape_point = 0, colour_point = "black" , size_point= 1){ 
 
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, file must be defined!\nThis data can be obtained using stilf predicates holds or occurs.")
  ensurer::ensure_that(EPSG_WGS84, !is.null(EPSG_WGS84), 
                       err_desc = "EPSG_WGS84 must be defined, if exists values of longitude and latitude (TRUE ou FALSE)! Default is TRUE")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette), 
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(shape_point, !is.null(shape_point), 
                       err_desc = "Define the shape point for events highlighted over map! Default is 0.")
  ensurer::ensure_that(colour_point, !is.null(colour_point), 
                       err_desc = "Define the colour point for events highlighted over map! Default is black.")  
  ensurer::ensure_that(size_point, !is.null(size_point), 
                       err_desc = "Define the size point for events highlighted over map! Default is 1.")
  
  input_data <- data_tb
  
  # create points  
  .createPointsEvents(input_data, EPSG_WGS84)
  
  ## Points event
  b <- data.frame(Reduce(rbind, points_events_map.list))

  rownames(b) <- NULL
  b <- data.frame(b) %>% dplyr::filter(b$w != "NA")
  b$x <- as.integer(b$x)
  b$y <- as.integer(b$y)
  b$w <- as.factor(b$w)
  b$z <- as.factor(b$z)
  map_events_df <- b ###
  
  map_events_df <- map_events_df[order(map_events_df$w),] # order by years
  rownames(map_events_df) <- seq(length=nrow(map_events_df)) # reset row numbers
  
  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(map_input_df$z))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!\n")
    } else {
      my_palette = RGB_color  
    }
  } else {
    # more colors
    colour_count = length(unique(map_input_df$z))
    my_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(name="Paired", n = 12))(colour_count)
  } 
  
  # plot only events
  g <- ggplot2::ggplot() +
    geom_raster(data=map_input_df, aes(map_input_df$x, map_input_df$y, fill=map_input_df$"z")) +
    geom_point(data=map_events_df, aes(x=map_events_df$x, y=map_events_df$y), shape=shape_point, colour = colour_point, size = size_point) + #  size=1.4
    scale_y_continuous(expand = c(0, 0), breaks = NULL) +
    scale_x_continuous(expand = c(0, 0), breaks = NULL) +
    facet_wrap("w") +
    coord_fixed(ratio = 1) + 
    #coord_fixed(ratio = 1/cos(mean(x)*pi/180)) +
    theme(legend.position = "bottom") +#, strip.text = element_text(size=10)) +
    xlab("") +
    ylab("") +
    scale_fill_manual(name="Legend:", values = my_palette)
    #scale_fill_brewer(name="Legend:", palette= "Paired")
  
  print(g)
 
}

# create points events
.createPointsEvents <- function(input_data, EPSG_WGS84){ 
  
  mapEvents_tb <- input_data 
 
  dates <- unique(lubridate::year(mapEvents_tb$end_date))
  indexLong <- which(colnames(mapEvents_tb) == "longitude")
  indexLat <- which(colnames(mapEvents_tb) == "latitude")
  indexLabel <- which(colnames(mapEvents_tb) == "label")
  
  # save points in environment
  points_events_map.list <- NULL
  points_events_map.list <<- list()
  
  for(x in 1:length(dates)){
   
    map <- filter(mapEvents_tb, grepl(dates[x], as.character(mapEvents_tb$end_date), fixed = TRUE))
    pts <- map[c(indexLong:indexLat,indexLabel)] # long, lat and class
    colnames(pts) <- c('x', 'y', 'z')
    
    if (EPSG_WGS84 == TRUE) {
      # converte to sinusoidal projection in case values in Longitude and Latitude
      d <- data.frame(x=pts$x, y=pts$y, z=pts$z, w=dates[x])
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
    points_events_map.list[[paste("ptsEvents_",dates[x], sep = "")]] <<- pts1
   
  }
  
}  


#' @title Plot Sequence Maps with STILF Events
#' @name stilf_plot_sequence_events
#' @aliases stilf_plot_sequence_events
#' @author Adeline M. Maciel
#' @docType data
#' 
#' @description Plot time series as sequence of lines over time 
#' 
#' @usage stilf_plot_sequence_events (data_tb = NULL, show_y_index = TRUE,
#' start_date = "2000-01-01", end_date = "2016-12-31")
#' 
#' @param data_tb        Tibble. A tibble with values longitude and latitude and other values
#' @param show_y_index   Boolean. TRUE/FALSE to show the index values in the axis y of the graphic
#' @param start_date     Date. A start date to plot in sequence in format (ymd), '2011-01-01'
#' @param end_date       Date. A end date to plot in sequence in format (ymd), '2013-01-01'
#' 
#' @keywords datasets
#' @return Plot sequence time series as lines
#' @import ggplot2
#' @importFrom ensurer ensure_that 
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
#' # open file JSON
#' input_tb_raw_json <- file_json %>% 
#'   stilf_fromJSON() 
#' input_tb_raw_json
#' 
#' # plot maps input data
#' stilf_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)
#' 
#' # define interval
#' time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")
#'  
#' # apply predicate occur
#' ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, 
#' object_properties = "Forest", event_time_intervals = time_ex1)
#' 
#' # events over input map
#' stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, 
#' custom_palette = FALSE, size_square = 1)
#' 
#' # plot sequence of events
#' stilf_plot_sequence_events(ts_occur1, show_y_index = TRUE,
#' start_date = "2000-01-01", end_date = "2016-12-31")
#' 
#'   
#'}
#'

stilf_plot_sequence_events <- function(data_tb = NULL, show_y_index = TRUE, start_date = "2000-01-01", end_date = "2016-12-31"){ 
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, file must be defined!\nThis data can be obtained using stilf_plot_maps_events().")
  ensurer::ensure_that(show_y_index, !is.null(show_y_index), 
                       err_desc = "Show y index label must be defined! Default is 'TRUE'")
  ensurer::ensure_that(start_date, !is.null(start_date), 
                       err_desc = "Start date must be defined! Default is '2000-01-01'")
  ensurer::ensure_that(end_date, !is.null(end_date), 
                       err_desc = "End date must be defined! Default is '2016-12-31'!")
  
  mapSeq <- data_tb
  mapSeq <- mapSeq[order(mapSeq$index),] # order by index
  
  mapSeq$start_date <- as.Date(mapSeq$start_date, format = '%Y-%m-%d')
  mapSeq$end_date <- as.Date(mapSeq$end_date, format = '%Y-%m-%d')
  
  data <- as.data.frame(mapSeq) # data from package datasets
  data$Category <- as.character(mapSeq$index) # this makes things simpler later
  
  g <- ggplot2::ggplot(data, aes(y = data$Category)) +
    labs(x = "Years", y = "Time series set") +
    theme_bw()+
    geom_segment(aes(x = data$"start_date", y = data$Category,
                     xend = data$"end_date", yend = data$Category,
                     color = data$"label"), size = 1.25) +
    
    geom_point(aes(x = data$"start_date", color =  data$"label"), size = 3, shape = 19) +
    geom_point(aes(x = data$"end_date", color = data$"label"), size = 3, shape = 19) +
    
    # define time period
    scale_x_date(limits=as.Date(c(start_date, end_date))) +
    labs(colour = "Legend:")
  
  # shows axis y label with index values from tibble
  if(show_y_index == TRUE){
    g <- g + theme(legend.position = "bottom", 
             #legend.text=element_text(size=10),
             legend.key = element_blank())  
  } else {
    g <- g + theme(legend.position = "bottom", 
             #legend.text=element_text(size=10),
             axis.text.y=element_blank(),
             legend.key = element_blank())  
  } 
  
  print(g)
 
}



#' @title Plot Barplot Maps with STILF Events
#' @name stilf_plot_barplot_events
#' @aliases stilf_plot_barplot_events
#' @author Adeline M. Maciel
#' @docType data
#' 
#' @description Plot barplot over time 
#' 
#' @usage stilf_plot_barplot_events (data_tb = NULL, 
#' custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250)
#' 
#' @param data_tb          Tibble. A tibble with values longitude and latitude and other values
#' @param custom_palette   Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE
#' @param RGB_color        Character. A vector with color names to map legend, for example, c("Green","Blue"). Default is the color brewer 'Paired'
#' @param pixel_resolution Numeric. Is a spatial resolution of the pixel. Default is 250 meters considering MODIS 250 m. See more at \url{https://modis.gsfc.nasa.gov/about/specifications.php}.
#' 
#' @keywords datasets
#' @return Plot a barplot in Y axis in square kilometers (Area km^2) = (Number of pixel *(pixel_resolution*pixel_resolution))/(1000*1000)
#' @import ggplot2
#' @importFrom ensurer ensure_that 
#' @importFrom lubridate year 
#' @importFrom scales hue_pal
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
#' # open file JSON
#' input_tb_raw_json <- file_json %>% 
#'   stilf_fromJSON() 
#' input_tb_raw_json
#' 
#' # plot maps input data
#' stilf_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)
#' 
#' # define interval
#' time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")
#'  
#' # apply predicate occur
#' ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, 
#' object_properties = "Forest", event_time_intervals = time_ex1)
#' 
#' # events over input map
#' stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, 
#' custom_palette = FALSE, size_square = 1)
#' 
#' # plot barplot of events
#' stilf_plot_barplot_events(ts_occur1, custom_palette = FALSE)
#' 
#' 
#'}
#'

stilf_plot_barplot_events <- function(data_tb = NULL, custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250){ 
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, file must be defined!\nThis data can be obtained using stilf_plot_maps_events().")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette), 
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(pixel_resolution, !is.null(pixel_resolution), 
                       err_desc = "pixel_resolution must be defined! Default is 250 meters on basis of MODIS image")  

  input_data <- data_tb
  input_data <- input_data[order(input_data$index),] # order by index
  
  #mapBar <- data.frame(table(input_data$w, input_data$z))
  mapBar <- data.frame(table(lubridate::year(input_data$end_date), input_data$label))
  
  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(mapBar$Var2))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n") 
    } else {
      my_palette = RGB_color  
    }
  } else {
    # more colors
    colour_count = length(unique(mapBar$Var2))
    my_palette = scales::hue_pal()(colour_count)
  } 
  
  g <- ggplot2::ggplot(mapBar,aes(x=mapBar$Var1, y=(mapBar$Freq*(pixel_resolution*pixel_resolution))/(1000*1000), fill=mapBar$Var2))+
    geom_bar(width = 0.7, stat="identity")+ #, position=position_dodge())+
    theme_bw()+
    ylab(expression(paste("Area ",km^{2}," = ((pixels number x pixel ", resolution^{2},")/",1000^{2},")")))+
    xlab("Years")+
    #scale_fill_brewer(name="Legend:", palette= "Paired") +
    scale_fill_manual(name="Legend:", values = my_palette) +
    theme(legend.position = "bottom", 
          #legend.text=element_text(size=10), 
          legend.key = element_blank())
  
  print(g)
  
}



