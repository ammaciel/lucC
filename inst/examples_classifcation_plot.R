
#####
# Example: classify a set of time series using a pattern then show plots
library(stilf)

stilf_starting_point()

library(sits)
# 
# zip_point_tb <- unzip("./inst/list_time_series_Area1_Sinop.zip")
# point_tb <- sits::sits_getdata(zip_point_tb)
# 
# ts_data <- point_tb

#ts_data <- sits_getdata("~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part.json")
ts_data <- sits_getdata("~/Desktop/ESTUDO_TESE/Studies/Sinop/Sinop_part.json")

# read a pattern table from a JSON file
#patterns_tb <- sits_getdata("./inst/patterns/patterns_Damien_Ieda_Rodrigo_14classes_3bands_Rename_Labels_Sep.json")
patterns_tb <- sits_getdata("~/Dropbox/TWDTWAmazoniaCerrado/Assinaturas/JSON/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")

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
ts_data

# classify with TWDTW and return a format to stilf
#res_carmem <- stilf_applyTWDTW(ts_data, patterns_tb, bands)
res_sinop <- stilf_applyTWDTW(ts_data, patterns_tb, bands)

#stilf_toJSON(res_carmem, path_json_file = "~/Desktop/Carmem_twdtw.json")
#stilf_toJSON(res_sinop, path_json_file = "~/Desktop/Sinop_twdtw.json")


res_carmem

# ##
# #stilf_toJSON(res,"~/Desktop/ESTUDO_TESE/Studies/Sinop/Sinop_classify_15classes_3bands.json")
# #remove columns and pass to stilf format
# new_area_Sinop <- res %>% 
#   dplyr::select(longitude,latitude,label,start_date,end_date,id,index) %>% 
#   stilf_data_preparation()
# 
# new_area_Sinop
# 
# # alter start_date and end_date to a especific range in order to extract events
# area_tb <- stilf_standard_date_events(data_tb = new_area_Sinop, month_year = "08", day_month = "15")
# 
# area_tb
# 
# # plot maps input data
# stilf_plot_maps_input(area_tb, EPSG_WGS84 = TRUE)
# 

#
stilf_plot_maps_input(res_carmem, EPSG_WGS84 = TRUE)

# remove data with 2000 in end_data
res_classification <- dplyr::filter(res_classification, 
                                    !grepl("2000", as.character(res_classification$end_date), 
                                           fixed = TRUE))

res_sinop2 <- dplyr::filter(res_sinop, 
                            !grepl("2017", as.character(res_sinop$end_date), 
                                   fixed = TRUE))




b <- c("Florest","Forest","Forest","Pasture1","Pasture2","Soybean")

gsub("[A-Z][1-9]:", "", string)

gsub("[A-Z][a-z],", "", b)


