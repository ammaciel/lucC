####
# Example: read a json file using lucC_fromJSON and 
# adequate data to extrat a lot of events
library(sits)
library(lucC)

lucC_starting_point()

#*********************************
# Classification time series using a set of temporal patterns
#*********************************

santa_carmem.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part_all.tb.RData"))
santa_carmem.tb

# select a sub set
sel <- read.csv(file = "~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/santacarmem_sel.csv", sep = ",", stringsAsFactors = FALSE)
sel

tb <- santa_carmem.tb

# select only points into this select region
indexLong <- which(colnames(sel) == "longitude")
indexLat <- which(colnames(sel) == "latitude")
coord <- dplyr::distinct(sel[indexLong:indexLat])
output.tb = tb[FALSE,]

for(x in 1:nrow(coord)){
  #x=1
  temp0 <- dplyr::filter(tb, grepl(coord[x,1], as.character(tb$longitude), fixed = TRUE) &
                           grepl(coord[x,2], as.character(tb$latitude), fixed = TRUE))
  
  output.tb <- dplyr::bind_rows(output.tb,temp0)
}  
output.tb  

example_ts <- output.tb
example_ts

#save(example_ts, file = "~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/example_ts.RData", compress = "xz")
#sits_toJSON(example_ts.tb, file = "~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/example_ts.tb.json")

####
library(lucC)
library(sits)

lucC_starting_point()

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

# # classification using sits_TWDTW
# example_sits_TWDTW <- sits_TWDTW(example_ts, patterns.tb, bands)
# example_sits_TWDTW
# 
# # convert example_sits_TWDTW from sits package to lucC format
# example_convert_sits_TWDTW <- lucC_TWDTW_fromSITS(data_tb = example_sits_TWDTW)
# example_convert_sits_TWDTW
# 
# plot maps
# lucC_plot_maps_input(example_convert_sits_TWDTW, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 

# classification using lucC_applyTWDTW
example_lucC_TWDTW <- lucC_TWDTW(example_ts, patterns.tb, bands)
example_lucC_TWDTW

# plot maps
lucC_plot_maps_input(example_lucC_TWDTW, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 



# example_TWDTW <- example_convert_sits_TWDTW
# # use lucC_toJSON despite of decimal digits
# save(example_TWDTW, file = "~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/example_TWDTW.RData", compress='bzip2')
# #write.table(example_TWDTW, "~/Desktop/example_TWDTW.csv", quote = FALSE, sep = ",", row.names = FALSE)
# lucC_toJSON(example_TWDTW, path_json_file = "~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/example_TWDTW.json")

data("example_TWDTW")
example_TWDTW

#-----------
#data.frame(unique(patterns.tb$label))
# cla <- c("Soybean_Pasture")
# patterns.tb <- dplyr::filter(patterns.tb, patterns.tb$label != cla)

## json have other attributes
#values <- c("Index", "ndvi", "evi", "nir") # 
# # remove other attributes
# santa_carmem.tb$time_series <- lapply(santa_carmem.tb$time_series,function(p){
#   p <- p[,values, drop = FALSE]
# })
# classify with TWDTW and return a format to lucC
#res <- lucC_applyTWDTW(santa_carmem.tb[1:3,], patterns.tb, bands)
#
## remove data with 2017 in end_data
# santa_carmem_TWDTW.tb <- dplyr::filter(santa_carmem_TWDTW.tb, 
#                                     !grepl("2017", as.character(santa_carmem_TWDTW.tb$end_date), fixed = TRUE))
#-----------


#*********************************
# Question examples
#*********************************
#---------------------------------
# Question 1 - Only events of Pasture
# o = geo-objects, the own df_input data.frame
#---------------------------------
library(lucC)

data("example_TWDTW")
example_TWDTW

# alter start_date and end_date to a especific range in order to extract events
example_1.tb <- example_TWDTW %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_1.tb

# p = properties of objects :
p1 <- "Pasture"

# t = interval:
t1 <- lucC_interval("2000-09-01","2017-03-01")

# Test occur for many time series
QuestionOccurs <- function(data_tb, p, t){
  
  tb <- data_tb 
  coord <- unique(tb$index)
  output.tb <- tb[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- tb[which(as.character(tb$index) == coord[x]),]
    
    if (nrow(event2 <- lucC_predicate_holds(temp, p1, t1)) >= 1
        
    ){
      temp0 <- rbind(event2)
    } else {
      temp0 <- NULL
    }
    output.tb <- dplyr::bind_rows(output.tb,temp0)
  }
  return(output.tb)
}

output.tb <- QuestionOccurs(example_1.tb, p = p1, t = t1)
output.tb

remove(t1,p1)

# plot results
lucC_plot_maps_input(example_1.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))

#plot events
lucC_plot_maps_events(output.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "black", size_point = 2.3) 

lucC_plot_barplot_events(output.tb, custom_palette = TRUE, RGB_color = "#929e6e", pixel_resolution = 250) 

lucC_plot_sequence_events(output.tb, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e") 

#lucC_plot_barplot_events(df_new[which(df_new$label == "Forest"),]) 


#---------------------------------
# Question 2 - Only one point
# o = geo-objects, the own df_input data.frame
#---------------------------------

data("example_TWDTW")
example_TWDTW

# select only one time serie with index equals 13
# alter start_date and end_date to a especific range in order to extract events
example_2.tb <- example_TWDTW %>% 
  dplyr::filter(., .$index == 13) %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_2.tb

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"

# t = interval:
t1 <- lucC_interval("2000-09-01","2004-09-01")
t2 <- lucC_interval("2004-09-01","2017-09-01")

# Test occur for one time serie
QuestionOccurs <- function(data_tb, p, t){
 
  output.tb <- data_tb[FALSE,]
  data_tb
  
  if (nrow(ev1 <- lucC_predicate_holds(data_tb, p1, t1)) >= 1 &
      nrow(ev2 <- lucC_predicate_holds(data_tb, p2, t2)) >= 1 &
      
      isTRUE(lucC_relation_meets(tail(lucC_interval(ev1$start_date, ev1$end_date), 1),
                                  head(lucC_interval(ev2$start_date, ev2$end_date),1)))
  ){
    temp0 <- rbind(ev1,ev2)
  } else {
    temp0 <- NULL
  }
  output.tb <- dplyr::bind_rows(output.tb,temp0)

  return(output.tb)
}

output.tb2 <- QuestionOccurs(example_2.tb, p = c(p1, p2), t = c(t1,t2))
output.tb2

remove(p1, p2, t1, t2)

# plot
lucC_plot_maps_input(example_2.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"))

lucC_plot_maps_events(output.tb2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"), shape_point = 4, colour_point = "blue", size_point = 8) 

lucC_plot_barplot_events(output.tb2, custom_palette = FALSE) 

lucC_plot_sequence_events(output.tb2, show_y_index = FALSE, end_date = "2017-03-01") 
#lucC_plot_barplot_events(output_df2[which(output_df2$label == "Forest"),]) 


#---------------------------------
# Question 3 - Only transition "Forest", "Pasture", "Single_cropping", "Double_cropping"
# o = geo-objects, the own df_input data.frame
#---------------------------------
library(lucC)

lucC_starting_point()

data("example_TWDTW")
example_TWDTW

example_3.tb <- example_TWDTW %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_3.tb

# p = properties of objects :
p1 <- c("Forest", "Pasture", "Single_cropping", "Double_cropping")

# t = interval:
t1 <- lucC_interval("2000-08-01","2017-03-01")

tb <- example_3.tb
output.tb3 <- tb[FALSE,]
coord <- unique(tb$index)

# Apply for each time series based on index
for(x in 1:length(coord)){
  temp.tb <- tb[which(as.character(tb$index) == coord[x]),]
  temp_final.tb <- lucC_event_transitions(temp.tb, properties = p1, time_intervals = t1)
  output.tb3 <- dplyr::bind_rows(output.tb3, temp_final.tb)
}
output.tb3

# plots
lucC_plot_maps_input(example_3.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))

lucC_plot_maps_events(output.tb3, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "blue", size_point = 2.3) 

lucC_plot_barplot_events(output.tb3, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), pixel_resolution = 250) 

lucC_plot_sequence_events(output.tb3, show_y_index = TRUE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 



