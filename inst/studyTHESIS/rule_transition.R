##########
library(stilf)

stilf_starting_point()

file_json = "~/Desktop/Sinop_postprocess.json"

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

#
#"Forest --> Pasture --> Cropping" 
#
####
# Test with one point
temp0.tb <- df[which(df$index == 18),]# 1134),]#1003),]
temp0.tb

temp_final.tb <- stilf_discover_transitions(temp0.tb, properties = c("Forest", "Pasture", "Cropping"))
temp_final.tb


####
# Test with more than one point
#temp0.tb <- df[which(df$index >= 1 & df$index <= 30),]

data_tb <- df
#data_tb <- temp0.tb # df

output.tb <- data_tb[FALSE,]

coord <- unique(data_tb$index)

# Forest --> Pasture --> Cropping - Type of transition
transition_string <- c("Forest", "Pasture", "Cropping")

for(x in 1:length(coord)){
#x=2
  temp.tb <- data_tb[which(as.character(data_tb$index) == coord[x]),]
  temp_final.tb <- stilf_event_transitions(temp.tb, properties = transition_string)
  output.tb <- bind_rows(output.tb, temp_final.tb)
  
}

stilf_plot_maps_input(df, EPSG_WGS84 = TRUE)
stilf_plot_maps_events(output.tb, EPSG_WGS84 = TRUE)
stilf_plot_sequence_events(output.tb)



########## Example 2
library(stilf)

stilf_starting_point()

# open a JSON file with data classified
input_tb <- "./inst/postprocess_Sinop.json" %>% 
  stilf_fromJSON() 

# if necessary, create a new column to rpeserve the old label
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

# create a tibble with the same column names 
output.tb <- df[FALSE,]
coord <- unique(df$index)

# Forest --> Pasture --> Cropping - Type of transition
transition_string <- c("Forest", "Pasture", "Cropping")

# Apply over all input data
for(x in 1:length(coord)){
  temp.tb <- df[which(as.character(df$index) == coord[x]),]
  temp_final.tb <- stilf_event_transitions(temp.tb, properties = transition_string)
  output.tb <- bind_rows(output.tb, temp_final.tb)
}
output.tb

# plot transitions discovered
stilf_plot_maps_input(df, EPSG_WGS84 = TRUE)
stilf_plot_maps_events(output.tb, EPSG_WGS84 = TRUE)
stilf_plot_sequence_events(output.tb)

