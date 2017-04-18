
############## Get time series from WTSS using sits package
library(stilf)
library(sits)

stilf_starting_point()

URL <- "http://www.dpi.inpe.br/tws/wtss"
sits_infoWTSS(URL)
coverage =  "mod13q1_512"
bands = c("ndvi", "evi", "red", "nir", "blue", "mir")

samples.tb <- sits_getdata(file = "./data/example_points_ts.csv", URL = URL, coverage =  coverage, bands = bands)
samples.tb

# save the input as json format - more than 8 digits
output_file = "~/Desktop/time_series_data.json"
stilf_toJSON(samples.tb, output_file)


############## Classify JSON using sits

# Example: read a json file using stilf_fromJSON and 
# perform classification over area using stis, and after saved as json file

library(stilf)
library(sits)

stilf_starting_point()

# open data with time series set
data("example_ts")
example_ts

# read a pattern table from a JSON file
patterns.tb <- sits_getdata("./inst/extdata/patterns/example_temporal_pattern.json")
patterns.tb

# plot patterns
sits_plot(patterns.tb, type = "patterns")

# only this bands have in patterns
bands <- c("ndvi", "evi", "nir")

# classification using sits_TWDTW
example_sits_TWDTW <- sits_TWDTW(example_ts, patterns.tb, bands)
example_sits_TWDTW

# convert example_sits_TWDTW from sits package to stilf format
example_convert_sits_TWDTW <- stilf_TWDTW_fromSITS(data_tb = example_sits_TWDTW)
example_convert_sits_TWDTW

#plot maps
stilf_plot_maps_input(example_convert_sits_TWDTW, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))

# # classification using stilf_applyTWDTW
# example_stilf_TWDTW <- stilf_TWDTW(example_ts, patterns.tb, bands)
# example_stilf_TWDTW

# # plot maps
# stilf_plot_maps_input(example_stilf_TWDTW, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 

# stilf_toGeoTIFF(res, path_raster_folder = "~/Desktop/raster/")


# ##### aux
# # convert labels to a vector of strings
# label_names <- matches$distances[[1]] %>% 
#   colnames() %>% 
#   dplyr::setdiff(c("year", "start_date", "end_date"))
# 
# shortest_dist <- res_to %>% 
#   dplyr::select(dplyr::one_of(label_names)) %>% 
#   t() %>% 
#   which.min()  
# 
# # dicover what a label match with this distance  
# label_win <- label_names[which(index(label_names) == shortest_dist)]


############## Plots

library(stilf)

stilf_starting_point()

# open a JSON file example
file_json = "./inst/extdata/patterns/example_TWDTW.json"

# open file JSON
input_tb_raw_json <- file_json %>%
  stilf_fromJSON()
input_tb_raw_json

# plot maps input data
stilf_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))

# define interval
time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")

# apply predicate occur
ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_raw_json, object_properties = "Forest", event_time_intervals = time_ex1)
ts_occur1

# events over input map
stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "black", size_point = 2.3)

stilf_plot_sequence_events(ts_occur1, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e") 

stilf_plot_barplot_events(ts_occur1, custom_palette = TRUE, RGB_color = "#929e6e", pixel_resolution = 250) 



###################
library(stilf)

stilf_starting_point()

# open data with time series set
data("example_TWDTW")
example_TWDTW

# save rasters in folder
stilf_toGeoTIFF (example_TWDTW, "~/Desktop/raster")



#############

library(stilf)

data("example_TWDTW")
example_TWDTW

# alter start_date and end_date to a especific range in order to extract events
example_1.tb <- example_TWDTW %>% 
  stilf_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_1.tb

