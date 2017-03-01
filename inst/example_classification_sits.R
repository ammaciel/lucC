####
# Example: Convert RData with time series to JSON format

library(stilf)
stilf_starting_point()

library(sits)
load("~/Desktop/ESTUDO_TESE/Studies/Area_Sinop/list_time_series_Area1_Sinop.RData")

# variable initialization
server <- "http://www.dpi.inpe.br/tws/wtss"
sits_infoWTSS(server)
wtss_data <- list_time_series
class <- "NoClass"
cv <- "mod13q1_512"

# create a tibble
ts_tibble <- sits_table()

for(i in 1:length(wtss_data)){
  
  ts_wtss <- wtss_data[i][[1]][[1]]$attributes*0.0001 #### fazer ISSO
  long <- wtss_data[i][[1]][[1]]$center_coordinate$longitude
  lat <- wtss_data[i][[1]][[1]]$center_coordinate$latitude
  
  # pass zoo to data.frama
  ts_tb <- fortify.zoo(ts_wtss)
  
  # store the sits table in a list
  ts_list        <- list()
  ts_list [[1]]  <- as_tibble (ts_tb)
  
  # add the row to the sits table
  ts_tibble <- add_row(ts_tibble, longitude   = long, 
                       latitude    = lat, 
                       start_date  = ts_tb[1,"Index"], 
                       end_date    = ts_tb[nrow(ts_tb),"Index"], 
                       label       = class,
                       coverage    = cv, 
                       time_series = ts_list )
  ts_tibble
}

ts_tibble

# use stilf_toJSON despite of decimal digits
stilf_toJSON(ts_tibble, path_json_file = "~/Desktop/ESTUDO_TESE/Studies/Area_Sinop/list_time_series_Area1_Sinop.json")



############## Classify JSON using sits

# json with time series area Sinop
# point_tb <- sits_getdata("~/Desktop/ESTUDO_TESE/Studies/Area_Sinop/list_time_series_Area1_Sinop.json")
# point_tb

# Example: read a json file using stilf_fromJSON and 
# perform classification over area using stis, and after saved as json file

library(stilf)

stilf_starting_point()

library(sits)

zip_point_tb <- unzip("./inst/list_time_series_Area1_Sinop.zip")
point_tb <- sits::sits_getdata(zip_point_tb)

ts_data <- point_tb

# read a pattern table from a JSON file
#patterns_tb <- sits_getdata("./inst/patterns/patterns_Damien_Ieda_Rodrigo_13classes_3bands_Sep.json")
patterns_tb <- sits_getdata("./inst/patterns/patterns_Damien_Ieda_Rodrigo_14classes_3bands_Rename_Labels_Sep.json")

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



#23016--22080
# use stilf_toJSON despite of decimal digits
#stilf_toJSON(res, path_json_file = "~/Desktop/ESTUDO_TESE/Studies/Area_Sinop/classification_14patterns_3bands_Area1_Sinop.json")

