####
# Example: read a json file using stilf_fromJSON and 
# perform some operations using predicates and plots

library(stilf)

stilf_starting_point()

file_json_zip = "./inst/area_Sinop/part_Sinop_classification_14patterns_3bands.zip"

input_tb_area_Sinop <- file_json_zip %>% 
  unzip() %>% 
  stilf_fromJSON() 

input_tb_area_Sinop

#remove columns and pass to stilf format
new_area_Sinop <- input_tb_area_Sinop %>% 
  select(longitude,latitude,label,start_date,end_date,id,index) %>% 
  stilf_data_preparation()

new_area_Sinop

classes <- unique(new_area_Sinop$label)

# plot maps input data
stilf_plot_maps_input(input_tb_area_Sinop, EPSG_WGS84 = TRUE)

# define interval
time_ex1 <- stilf_interval("2002-01-01", "2014-01-01")

# using occur for a class from classes variable
ts_occur1 <- stilf_predicate_occur(geo_objects = input_tb_area_Sinop, object_properties = "Soybean_Comerc1", event_time_intervals = time_ex1)

ts_occur2 <- stilf_predicate_occur(geo_objects = input_tb_area_Sinop, object_properties = "Forest", event_time_intervals = time_ex1)

# events over input map
stilf_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE, size_square = 1)
stilf_plot_maps_events(ts_occur2, EPSG_WGS84 = TRUE, size_square = 1)

# plot sequence of events
stilf_plot_sequence_events(ts_occur1, start.date = "2000-01-01", end.date = "2016-12-31")
stilf_plot_sequence_events(ts_occur2, start.date = "2000-01-01", end.date = "2016-12-31")

# plot barplot of events
stilf_plot_barplot_events(ts_occur1)
stilf_plot_barplot_events(ts_occur2)
