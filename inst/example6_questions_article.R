####
# Example: read a json file using stilf_fromJSON and 
# adequate data to extrat a lot of events

library(stilf)

stilf_starting_point()

file_json = "./inst/area_Sinop/part_Sinop_postprocess.json"

input_tb <- file_json %>% 
  stilf_fromJSON() 

input_tb

# Forest --> Pasture --> Cropping
df <- input_tb
df$"label2" <- df$label

# change label for 
for (x in 1:nrow(df)){
  if (df$label2[x] == "Savanna") {
    df$"label"[x] = "Cerrado"
  }
  else if (df$label2[x] %in% c("Pasture1","Pasture2")) {
    df$"label"[x] = "Pasture"
  }
  else if (df$label2[x] %in% c("Soybean_Fallow1", "Soybean_Fallow2", 
                               "Soybean_NonComerc1", "Soybean_NonComerc2", 
                               "Soybean_Comerc1", "Soybean_Comerc2", 
                               "Soybean_Pasture", "NonComerc_Cotton", 
                               "Fallow_Cotton", "Soybean_Cotton")) {
    df$"label"[x] = "Cropping"
  }
}

df

###########################
#### Question to journal
## Question 1 - Which "Forest" areas haven't been replaced by other croppings?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Forest"

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
    output_df <- bind_rows(output_df,temp0)
  }
  return(output_df)
}

output_df <- QuestionOccurs(df, p = p1, t = t1)
remove(t1)
remove(p1)


# remove duplicated rows
length(which(duplicated(output_df)))
#output_df <- output_df[!duplicated(output_df),]

stilf_plot_maps_input(df, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#CCCC00", "#FFB266", "#009900", "#FFFFCC", "#66CC00")) 
stilf_plot_maps_input(df, EPSG_WGS84 = TRUE, custom_palette = FALSE) 

stilf_plot_maps_events(output_df, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("blue", "red", "green", "yellow", "pink")) 
stilf_plot_barplot_events(output_df, custom_palette = FALSE, RGB_color = c("green")) 
stilf_plot_sequence_events(output_df, show_y_index = FALSE, end_date = "2017-03-01" ) 

#stilf_plot_barplot_events(df_input_new[which(df_input_new$label == "Forest"),]) 



###########################
#### Question to journal
## Question 2 - Which "Forest" areas haven't been replaced by other croppings?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Forest"

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
    output_df <- bind_rows(output_df,temp0)
  }
  return(output_df)
}

output_df <- QuestionOccurs(df, p = p1, t = t1)
remove(t1)
remove(p1)


# remove duplicated rows
length(which(duplicated(output_df)))
#output_df <- output_df[!duplicated(output_df),]

stilf_plot_maps_input(df, EPSG_WGS84 = TRUE) 
stilf_plot_maps_events(output_df, EPSG_WGS84 = TRUE) 
stilf_plot_barplot_events(output_df) 
stilf_plot_sequence_events(output_df, show_y_index = FALSE,end_date = "2017-03-01" ) 

#stilf_plot_barplot_events(df_input_new[which(df_input_new$label == "Forest"),]) 




