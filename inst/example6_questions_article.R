####
# Example: read a json file using stilf_fromJSON and 
# adequate data to extrat a lot of events
library(sits)
library(stilf)

stilf_starting_point()

#--------------
# Classification time series using a set of temporal patterns
#--------------

# ita.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_stilf.tb.RData"))
# ita.tb
# ita.tb2 <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_all.tb.RData"))
# ita.tb2

santa_carmem.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part_all.tb.RData"))
santa_carmem.tb

# read a pattern table from a JSON file
patterns.tb <- sits_getdata("~/Dropbox/TWDTWAmazoniaCerrado/Assinaturas/JSON/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")
data.frame(unique(patterns.tb$label))
cla <- c("Soybean_Pasture")
patterns.tb <- dplyr::filter(patterns.tb, patterns.tb$label != cla)

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
save(santa_carmem_TWDTW.tb, file = "~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part_TWDTW_stilf.tb.RData")

# see maps
stilf_plot_maps_input(santa_carmem_TWDTW.tb, EPSG_WGS84 = TRUE, custom_palette = FALSE) 



#--------------
# Rename for new labels before make questions
#--------------

santa_carmem_TWDTW.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part_TWDTW_stilf.tb.RData"))
santa_carmem_TWDTW.tb

# alter start_date and end_date to a especific range in order to extract events
Santa_Carmem_TWDTW_renamed.tb <- stilf_standard_date_events(data_tb = santa_carmem_TWDTW.tb, month_year = "09", day_month = "01")

Santa_Carmem_TWDTW_renamed.tb <- stilf_standard_date_events(data_tb = santa_carmem_TWDTW.tb, month_year = "09", day_month = "01")



# Forest --> Pasture --> Cropping
df <- Santa_Carmem_TWDTW_renamed.tb

# amount of data for class
data.frame(table(df$label))

df$"label2" <- df$label

# create progress bar
progress_bar <- txtProgressBar(min = 0, max = nrow(df), style = 3)

# change label for 
for (x in 1:nrow(df)){
  Sys.sleep(0.0)
  
  if (df$label2[x] == "Savanna") {
    df$"label"[x] = "Cerrado"
  } else if (df$label2[x] %in% c("Pasture1","Pasture2")) {
    df$"label"[x] = "Pasture"
  } else if (df$label2[x] %in% c("Soybean_Fallow1", "Soybean_Fallow2", 
                               "Fallow_Cotton")) {
    df$"label"[x] = "Single_cropping"
  } else if (df$label2[x] %in% c("Soybean_NonComerc1", "Soybean_NonComerc2", 
                               "Soybean_Comerc1", "Soybean_Comerc2", 
                               "NonComerc_Cotton", "Soybean_Cotton")) {
    df$"label"[x] = "Double_cropping"
  }
  # update progress bar
  setTxtProgressBar(progress_bar, x)
}
close(progress_bar)

df
# amount of data for class
data.frame(table(df$label))

# see maps
stilf_plot_maps_input(df, EPSG_WGS84 = TRUE, custom_palette = FALSE) 
stilf_plot_maps_input(df, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) # , "#66CC00" secondary_vegetation



##########
########################################
#+++++++++++++++++++++++++++++
# Secondary_vegetation
#+++++++++++++++++++++++++++++ 
# AREA "Part_Sinop" - IJGIS - pattern_classification_14patterns_3bands_Area1_Sinop
## Question - Which "Forest" areas have been turned in secondary-vegetation after anyclass? 
# o = geo-objects, the own df_input data.frame

# p = properties of objects 
p1 <- "Forest" 

p2 <- "Cerrado"
p3 <- "Pasture"
p4 <- "Single_cropping"
p5 <- "Double_cropping"

# te = interval:
t1 <- stilf_interval("2000-09-01","2001-09-01")
t2 <- stilf_interval("2001-09-01","2016-09-01")

##Luiz, quando você puder  poderia compartilhar a pasta TWDTWAmazoniaCerrado com Rodrigo, o e-mail dele é esse: rodrigo_anz@yahoo.com.br

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  # create progress bar
  progress_bar <- txtProgressBar(min = 0, max = length(coord), style = 3)###
  
  for(x in 1:length(coord)){
    # x=1
    Sys.sleep(0.0)###
    
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if ((nrow(ev1.in1 <- stilf_predicate_occur(temp, p1, t1)) >= 1 | #forest in first interval
         
         nrow(ev2.in1 <- stilf_predicate_occur(temp, p2, t1)) >= 1 | # other classes in first interval
         nrow(ev3.in1 <- stilf_predicate_occur(temp, p3, t1)) >= 1 |
         nrow(ev4.in1 <- stilf_predicate_occur(temp, p4, t1)) >= 1 |
         nrow(ev5.in1 <- stilf_predicate_occur(temp, p5, t1)) >= 1 
         
    ) 
    &
    (	  nrow(ev2.in2 <- stilf_predicate_occur(temp, p2, t2)) >= 1 | # other classes in second interval
        nrow(ev3.in2 <- stilf_predicate_occur(temp, p3, t2)) >= 1 |
        nrow(ev4.in2 <- stilf_predicate_occur(temp, p4, t2)) >= 1 |
        nrow(ev5.in2 <- stilf_predicate_occur(temp, p5, t2)) >= 1 
    ) 
    &
    nrow(ev1.in2 <- stilf_predicate_occur(temp, p1, t2)) >= 1 # if occur forest in second interval 
    & 
    # verify if forest discovered in interval 2 occur after start_date other classes. Or f is different from first year 
    # If TRUE, this forest is a secondary_vegetation  
    nrow(event3 <- ev1.in2[which(ev1.in2$start_date > min(
      ev2.in2$start_date, ev3.in2$start_date,
      ev4.in2$start_date, ev5.in2$start_date, na.rm=TRUE) | 
      (ev1.in2$label != head(temp$label,1))),]) >=1 
    
    ){
      temp0 <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1,
                     ev2.in2, ev3.in2, ev4.in2, ev5.in2,
                     event3)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
    
    # update progress bar
    setTxtProgressBar(progress_bar, x)###
    
  }
  
  close(progress_bar)###
  
  return(output_df)
}

# verify using function
data_sec_veg_occurs <- QuestionOccurs(area_tb, p = c(p1,p2,p3,p4,p5), t = c(t1,t2))

rm(list = ls(pattern="^p[0-9]")) # remove classes
remove(t1,t2) # remove intervals
##########



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




