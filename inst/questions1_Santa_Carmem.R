####
# Example: read a json file using stilf_fromJSON and 
# adequate data to extrat a lot of events
library(sits)
library(stilf)

stilf_starting_point()

#*********************************
# Classification time series using a set of temporal patterns
#*********************************

# ita.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_stilf.tb.RData"))
# ita.tb
# ita.tb2 <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_all.tb.RData"))
# ita.tb2

santa_carmem.tb <- get(load("./inst/area_Santa_Carmem/SantaCarmem_part_all.tb.RData"))
santa_carmem.tb

# read a pattern table from a JSON file
patterns.tb <- sits_getdata("./inst/patterns/temporal_pattern_example.json")
data.frame(unique(patterns.tb$label))
# cla <- c("Soybean_Pasture")
# patterns.tb <- dplyr::filter(patterns.tb, patterns.tb$label != cla)

# only this bands have in patterns
bands <- c("ndvi", "evi", "nir")

# plot patterns
sits_plot(patterns.tb, type = "patterns")

# json have other attributes
values <- c("Index", "ndvi", "evi", "nir") # 

# # remove other attributes
# santa_carmem.tb$time_series <- lapply(santa_carmem.tb$time_series,function(p){
#   p <- p[,values, drop = FALSE]
# })

# classify with TWDTW and return a format to stilf
#res <- stilf_applyTWDTW(santa_carmem.tb[1:3,], patterns.tb, bands)
santa_carmem_TWDTW.tb <- stilf_applyTWDTW(santa_carmem.tb, patterns.tb, bands)
santa_carmem_TWDTW.tb

# use stilf_toJSON despite of decimal digits
#save(santa_carmem_TWDTW.tb, file = "~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part_TWDTW_stilf.tb.RData")
stilf_data <- santa_carmem_TWDTW.tb
#save(stilf_data, file = "~/Desktop/example_data_TWDTW.RData")

# see maps
stilf_plot_maps_input(stilf_data, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 


#*********************************
# Rename for new labels before make questions
#*********************************
library(stilf)

#santa_carmem_TWDTW.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part_TWDTW_stilf.tb.RData"))
#santa_carmem_TWDTW.tb <- get(load("~/Desktop/SantaCarmem/SantaCarmem_part_TWDTW_stilf.tb.RData"))

data("example_data_TWDTW")
examples.tb <- stilf_data

# alter start_date and end_date to a especific range in order to extract events
examples_1.tb <- stilf_standard_date_events(data_tb = examples.tb, month_year = "09", day_month = "01")
examples_1.tb

# Forest --> Pasture --> Cropping
df <- examples_1.tb

# amount of data for class
data.frame(table(df$label))


#*********************************
# Question examples
#*********************************

df_new <- examples.tb

#---------------------------------
# Question 1 - Which "Forest" areas haven't been replaced by other croppings?
# o = geo-objects, the own df_input data.frame
#---------------------------------

# p = properties of objects :
p1 <- "Pasture"

# t = interval:
t1 <- stilf_interval("2000-09-01","2017-03-01")

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if (nrow(event2 <- stilf_predicate_occur(temp, p1, t1)) >= 1
        
    ){
      temp0 <- rbind(event2)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
  }
  return(output_df)
}

output_df <- QuestionOccurs(examples_1.tb, p = p1, t = t1)
remove(t1)
remove(p1)

# remove duplicated rows
length(which(duplicated(output_df)))
# output_df <- output_df[!duplicated(output_df),]

stilf_plot_maps_input(examples_1.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) # secondary_vegetation

#png(filename = "~/Desktop/fig3_only_forest.png", width = 8, height = 8, units = 'in', res = 300)
stilf_plot_maps_events(output_df, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "black", size_point = 1) 
#dev.off()

#png(filename = "~/Desktop/fig3_barplot_forest.png", width = 8, height = 6, units = 'in', res = 300)
stilf_plot_barplot_events(output_df, custom_palette = TRUE, RGB_color = "#929e6e", pixel_resolution = 250) 
#dev.off()

stilf_plot_sequence_events(output_df, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e") 

#stilf_plot_barplot_events(df_new[which(df_new$label == "Forest"),]) 


#---------------------------------
# Question 2 - Which "Forest" areas have been replaced by Pasture after 2001?
# o = geo-objects, the own df_input data.frame
#---------------------------------

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"

# t = interval:
t1 <- stilf_interval("2000-09-01","2001-09-01")
t2 <- stilf_interval("2001-09-01","2017-09-01")

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if (nrow(ev1 <- stilf_predicate_occur(temp, p1, t1)) >= 1 &
        nrow(ev2 <- stilf_predicate_occur(temp, p2, t2)) >= 1 &
        
        isTRUE(stilf_relation_following(tail(stilf_interval(ev1$start_date, ev1$end_date)),
                                        head(stilf_interval(ev2$start_date, ev2$end_date))))
        
    ){
      temp0 <- rbind(ev1,ev2)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
  }
  return(output_df)
}

output_df2 <- QuestionOccurs(df_new, p = c(p1, p2), t = c(t1,t2))
remove(t1, t2)
remove(p1, p2)


# remove duplicated rows
length(which(duplicated(output_df2)))
#output_df2 <- output_df2[!duplicated(output_df2),]

stilf_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#66CC00", "#f5e7a1")) # secondary_vegetation

stilf_plot_maps_events(output_df2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) 

stilf_plot_barplot_events(output_df2, custom_palette = FALSE) 

stilf_plot_sequence_events(output_df2, show_y_index = FALSE, end_date = "2017-03-01") 
#stilf_plot_barplot_events(output_df2[which(output_df2$label == "Forest"),]) 

#---------------------------------
# Question 3 - Which "Forest" areas have been replaced by Pasture, Double cropping or single cropping after 2001?
# o = geo-objects, the own df_input data.frame
#---------------------------------

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"
p3 <- "Single_cropping"
p4 <- "Double_cropping"

# t = interval:
t1 <- stilf_interval("2000-09-01","2001-09-01")
t2 <- stilf_interval("2001-09-01","2017-09-01")

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if (nrow(ev1 <- stilf_predicate_occur(temp, p1, t1)) >= 1 &
        nrow(ev2 <- stilf_predicate_occur(temp, p2, t2)) >= 1 &
        nrow(ev3 <- stilf_predicate_occur(temp, p3, t2)) >= 1 &
        nrow(ev4 <- stilf_predicate_occur(temp, p4, t2)) >= 1 &
        
        isTRUE(stilf_relation_following(tail(stilf_interval(ev1$start_date, ev1$end_date)),
                                        head(stilf_interval(ev2$start_date, ev2$end_date)))) |
        isTRUE(stilf_relation_following(tail(stilf_interval(ev1$start_date, ev1$end_date)),
                                        head(stilf_interval(ev3$start_date, ev3$end_date)))) |
        isTRUE(stilf_relation_following(tail(stilf_interval(ev1$start_date, ev1$end_date)),
                                        head(stilf_interval(ev4$start_date, ev4$end_date))))
        
    ){
      temp0 <- rbind(ev1,ev2, ev3, ev4)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
  }
  return(output_df)
}

output_df2 <- QuestionOccurs(df_new, p = c(p1, p2, p3, p4), t = c(t1,t2))
remove(t1, t2)
remove(p1, p2, p3, p4)


# remove duplicated rows
length(which(duplicated(output_df2)))
#output_df2 <- output_df2[!duplicated(output_df2),]

png(filename = "~/Desktop/fig2_secondary_vegetation.png", width = 8, height = 8, units = 'in', res = 300)
stilf_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) # secondary_vegetation
dev.off()

png(filename = "~/Desktop/fig4_forest_to_others.png", width = 8, height = 8, units = 'in', res = 300)
stilf_plot_maps_events(output_df2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) 
dev.off()

png(filename = "~/Desktop/fig4_barplot_forest_others.png", width = 8, height = 6, units = 'in', res = 300)
stilf_plot_barplot_events(output_df2, custom_palette = TRUE, RGB_color = c( "#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 
dev.off()

stilf_plot_sequence_events(output_df2, show_y_index = FALSE, end_date = "2017-03-01") 

#stilf_plot_barplot_events(output_df2[which(output_df2$label == "Forest"),]) 

#---------------------------------
# Question 4 - Which "Forest" areas have been replaced by Pasture after in some time interval?
# o = geo-objects, the own df_input data.frame
#---------------------------------
# Forest --> Pasture --> Cropping - Type of transition

#transition_string <- c("Pasture","Cropping","Pasture","Cropping","Pasture","Cropping","Pasture","Cropping","Pasture","Cropping")
#transition_string <- c("Forest", "Cerrado", "Secondary_vegetation", "Cropping", "Pasture")

# p = properties of objects :
p1 <- c("Forest", "Pasture", "Single_cropping", "Double_cropping")

# t = interval:
t1 <- stilf_interval("2000-08-01","2017-03-01")

df <- examples_1.tb
# create a tibble with the same column names 
output_tb2 <- df[FALSE,] # Always run this 
coord <- unique(df$index)

# Apply over all input data
for(x in 1:length(coord)){
  temp.tb <- df[which(as.character(df$index) == coord[x]),]
  temp_final.tb <- stilf_event_transitions(temp.tb, properties = p1, time_intervals = t1)
  output_tb2 <- dplyr::bind_rows(output_tb2, temp_final.tb)
}
output_tb2

# plots
stilf_plot_maps_input(df, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) # secondary_vegetation

#png(filename = "~/Desktop/fig5_transitions.png", width = 8, height = 8, units = 'in', res = 300)
stilf_plot_maps_events(output_tb2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "blue", size_point = 1) 
#dev.off()

stilf_plot_barplot_events(output_tb2, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), pixel_resolution = 250) 

#png(filename = "~/Desktop/fig5_sequence.png", width = 8, height = 6, units = 'in', res = 300)
stilf_plot_sequence_events(output_tb2, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 
#dev.off()


