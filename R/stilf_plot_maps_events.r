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
#' @usage stilf_plot_maps_events (data_tb = NULL, EPSG_WGS84 = FALSE, size_square = 1)
#' 
#' @param data_tb       Tibble. A tibble with values longitude and latitude and other values
#' @param EPSG_WGS84    Character. A reference coordinate system. If TRUE, the values of latitude and longitude alredy use this coordinate system, if FALSE, the data set need to be transformed
#' @param size_square   Numeric. Is a size of the square around pixels in plot. Default is 1
#' 
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
#' # define interval
#' time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")
#'  
#' # apply predicate occur
#' ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, 
#' object_properties = "Forest", event_time_intervals = time_ex1)
#' 
#' # events over input map
#' stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, size_square = 1)
#' 
#'}
#'

# plot maps with events
stilf_plot_maps_events <- function(data_tb = NULL, EPSG_WGS84 = FALSE, size_square=1){ 
  
  if (!is.null(data_tb)) {
    input_data <- data_tb
  } else {
    stop("\nFile must be defined!\nThis data can be obtained using stilf predicates holds or occurs\n")
  }
  
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
  
  # plot only events
  g <- ggplot2::ggplot() +
    geom_raster(data=map_input_df, aes(map_input_df$x, map_input_df$y, fill=map_input_df$"z")) +
    geom_point(data=map_events_df, aes(x=map_events_df$x, y=map_events_df$y), shape=0, colour = "black" ) + #, size=sizeSquare) + #  size=1.4
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
 
}

# create points events
.createPointsEvents <- function(input_data, EPSG_WGS84){ 
  
  mapEvents_tb <- input_data 
 
  # start here
  dates <- unique(format(as.Date(mapEvents_tb$end_date), format = '%Y'))
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
#' @usage stilf_plot_sequence_events (data_tb = NULL, 
#' start.date = "2000-01-01", end.date = "2016-12-31")
#' 
#' @param data_tb     Tibble. A tibble with values longitude and latitude and other values
#' @param start.date  Date. A start date to plot in sequence in format (ymd), '2011-01-01'
#' @param end.date    Date. A end date to plot in sequence in format (ymd), '2013-01-01'
#' 
#' @keywords datasets
#' @return Plot sequence time series as lines
#' @import ggplot2 
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
#' # define interval
#' time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")
#'  
#' # apply predicate occur
#' ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, 
#' object_properties = "Forest", event_time_intervals = time_ex1)
#' 
#' # events over input map
#' stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, size_square = 1)
#' 
#' # plot sequence of events
#' stilf_plot_sequence_events(ts_occur1, start.date = "2000-01-01", 
#' end.date = "2016-12-31")
#' 
#'   
#'}
#'

stilf_plot_sequence_events <- function(data_tb = NULL, start.date = "2000-01-01", end.date = "2016-12-31"){ 
  
  if (!is.null(data_tb)) {
    mapSeq <- data_tb
  } else {
    stop("\nFile must be defined!\nThis data can be obtained using stilf_plot_maps_events()\n")
  }
  
  mapSeq$start_date <- as.Date(mapSeq$start_date, format = '%Y-%m-%d')
  mapSeq$end_date <- as.Date(mapSeq$end_date, format = '%Y-%m-%d')
  
  data <- as.data.frame(mapSeq) # data from package datasets
  data$Category <- as.character(mapSeq$id) # this makes things simpler later
  
  g <- ggplot2::ggplot(data, aes(y = data$Category)) +
    labs(x = "Years", y = "Time series set") +
    theme_bw()+
    geom_segment(aes(x = data$"start_date", y = data$Category,
                     xend = data$"end_date", yend = data$Category,
                     color = data$"label"), size = 1.25) +
    
    geom_point(aes(x = data$"start_date", color =  data$"label"), size = 3, shape = 19) +
    geom_point(aes(x = data$"end_date", color = data$"label"), size = 3, shape = 19) +
    
    # define time period
    scale_x_date(limits=as.Date(c(start.date, end.date))) +
    #scale_color_discrete(name = "Legend:") +
    scale_color_brewer(name="Legend:", palette= "Set3") +
    theme(legend.position = "bottom", 
          legend.text=element_text(size=10),
          axis.text.y=element_blank(),
          legend.key = element_blank())
  
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
#' @usage stilf_plot_barplot_events (data_tb = NULL)
#' 
#' @param data_tb     Tibble. A tibble with values longitude and latitude and other values
#' 
#' @keywords datasets
#' @return Plot a barplot in Y axis in (Area km^2) = (Freq*(250*250))/(1000*1000)
#' @import ggplot2 
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
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
#' stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, size_square = 1)
#' 
#' # plot barplot of events
#' stilf_plot_barplot_events(ts_occur1)
#' 
#' 
#'}
#'

stilf_plot_barplot_events <- function(data_tb = NULL){ 
  
  if (!is.null(data_tb)) {
    input_data <- data_tb
  } else {
    stop("\nFile must be defined!\nThis data can be obtained using stilf_plot_maps_events()\n")
  }
  
  #mapBar <- data.frame(table(input_data$w, input_data$z))
  mapBar <- data.frame(table(format(as.Date(input_data$end_date), format = '%Y'), input_data$label))
  
  g <- ggplot2::ggplot(mapBar,aes(x=mapBar$Var1, y=(mapBar$Freq*(250*250))/(1000*1000), fill=mapBar$Var2))+
    geom_bar(width = 0.7, stat="identity")+ #, position=position_dodge())+
    theme_bw()+
    ylab(expression("Area km"^{2}))+
    xlab("Years")+
    scale_fill_brewer(name="Legend:", palette= "Set3") +
    theme(legend.position = "bottom", 
          legend.text=element_text(size=10), 
          legend.key = element_blank())
  
  print(g)
  
}



