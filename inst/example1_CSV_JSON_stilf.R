
#' example_json_file
#' @name example_json_file
#'
#' @description Dataset of example with input data in format JSON to run some functions this package
NULL


#' example_json_Sinop_part
#' @name example_json_Sinop_part
#'
#' @description Dataset of example with input data in format JSON to run some functions this package. With data from a particular region from Sinop municipality, Mato Grosso, Brazil.
NULL


###############
## Ctrl+Shift+M %>% 
## Alt (-) <- 
###############

#####
# Example: read a csv as STILF format
library(stilf)

stilf_starting_point()

file = "./data/example_csv_file.csv"

input_tb_raw_csv <- file %>% 
  read.csv(sep = ",", header = TRUE) %>% 
  stilf_data_preparation()
input_tb_raw_csv

#####
# Example: read a csv as using stilf_fromCSV from package stilf

library(stilf)

stilf_starting_point()

file_csv = "./data/example_csv_file.csv"

input_tb_csv <- file_csv %>% 
  stilf_fromCSV(separator = ",", header_file = TRUE) #%>% 
input_tb_csv

# save the input as json format
output_file = "~/Desktop/example_json_Sinop_part.json"
stilf_toJSON(input_tb_csv, output_file)

#####
# Example: apply some operations use Allen's relations

library(stilf)

stilf_starting_point()

# define some intervals to example
time1 <- stilf_interval("2011-09-01","2011-10-01")
time2 <- stilf_interval("2011-09-15","2011-11-01")
time3 <- stilf_interval("2011-10-01","2011-11-01")
time4 <- stilf_interval("2011-10-01","2011-11-01")

# verify some interval are TRUE or FALSE using Allen's relations
stilf_relation_before(time1,time4)

stilf_relation_overlaps(time1,time2)

stilf_relation_following(time1,time3)

#####
# Example: read a json file using stilf_fromJSON and 
# perform some operations using predicates

library(stilf)

stilf_starting_point()

json_file = "./inst/example_json_file.json"

input_tb_json <- json_file %>% 
  stilf_fromJSON()  
input_tb_json

# example of application
time_ex1 <- stilf_interval("2001-01-01", "2003-01-01")
time_ex1
time_ex2 <- stilf_interval("2005-01-01", "2010-01-01")
time_ex2

# object_properties
properties <- "Soybean_Fallow"

# example predicate holds
stilf_predicate_holds(geo_objects = input_tb_json, object_properties = "Forest", time_intervals = time_ex1)

stilf_predicate_holds(geo_objects = input_tb_json, object_properties = properties, time_intervals = time_ex2)


#####
# Example: read a json file using stilf_fromJSON and 
# perform some operations using predicates and plots

library(stilf)

stilf_starting_point()

file_json = "./inst/example_json_Sinop_part.json"
input_tb_raw_json <- file_json %>% 
  stilf_fromJSON() 
input_tb_raw_json

# plot maps input data
stilf_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)

# define interval
time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")

# using occur
ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, object_properties = "Soybean_Fallow", event_time_intervals = time_ex1)

ts_occur2 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, object_properties = "Forest", event_time_intervals = time_ex1)

# events over input map
stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, size_square = 1)
stilf_plot_maps_events(ts_occur2, EPSG_WGS84 = TRUE, size_square = 1)

# plot sequence of events
stilf_plot_sequence_events(ts_occur1, start.date = "2000-01-01", end.date = "2016-12-31")
stilf_plot_sequence_events(ts_occur2, start.date = "2000-01-01", end.date = "2016-12-31")

# plot barplot of events
stilf_plot_barplot_events(ts_occur1)
stilf_plot_barplot_events(ts_occur2)


#####
# Example: save images as GeoTIFF file to open in GIS after
library(stilf)

stilf_starting_point()

file_json = "./inst/example_json_Sinop_part.json"
input_tb_raw_json <- file_json %>% 
  stilf_fromJSON() 
input_tb_raw_json

# save rasters in folder 
stilf_toGeoTIFF (input_tb_raw_json, "~/Desktop/raster")

# #------------------ verificar depois
# pts <- sp::spsample(pts,type="regular",cellsize=231.6564)
# pts1 <-  points2grid(pts, tolerance = 0.186495)
# sp::points2grid(SpatialPoints(pts), tolerance = 0.186495)
# sp::fullgrid(pts)=FALSE 
# sp::gridded(pts) = TRUE
# #------------------


#####
# Example: test with combination of relations and predicates
library(stilf)

stilf_starting_point()

file_json = "./inst/example_json_Sinop_part.json"
input_tb_raw_json <- file_json %>% 
  stilf_fromJSON() 
input_tb_raw_json











