
#####
# Example: classify a set of time series using a pattern then show plots
library(stilf)

stilf_starting_point()

library(sits)

zip_point_tb <- unzip("./inst/list_time_series_Area1_Sinop.zip")
point_tb <- sits::sits_getdata(zip_point_tb)

ts_data <- point_tb

# read a pattern table from a JSON file
patterns_tb <- sits_getdata("./inst/patterns/patterns_Damien_Ieda_Rodrigo_14classes_3bands.json")

# only this bands have in patterns
bands <- c("ndvi", "evi", "nir")

# plot patterns
sits_plot(patterns_tb, type = "patterns")

# json have other attributes
values <- c("Index", "ndvi", "evi", "nir") # 

# remove other attributes
ts_data$time_series <- lapply(ts_data$time_series,function(p){
  p <- p[,values, drop = FALSE]
})

# classify with TWDTW and return a format to stilf
res <- stilf_applyTWDTW(ts_data, patterns_tb, bands)

res

stilf_plot_maps_input(res, EPSG_WGS84 = TRUE)

stilf_plot_maps_events(res, EPSG_WGS84 = TRUE)

stilf_plot_sequence_events(res)

stilf_toGeoTIFF(res, path_raster_folder = "~/Desktop/rraa/")
