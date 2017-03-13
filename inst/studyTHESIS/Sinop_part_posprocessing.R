#####
# Example: classify a set of time series using a pattern then show plots
library(stilf)

stilf_starting_point()

sinop.tb <- stilf_fromJSON("~/Desktop/Sinop_twdtw.json")
sinop.tb

unique(lubridate::year(sinop.tb$end_date))
unique(lubridate::year(sinop.tb$start_date))

stilf_plot_maps_input(sinop.tb, EPSG_WGS84 = TRUE)

#remove columns and pass to stilf format
new_area_Sinop <- sinop.tb %>% 
  select(longitude,latitude,label,start_date,end_date,id,index) %>% 
  stilf_data_preparation()

new_area_Sinop

# alter start_date and end_date to a especific range in order to extract events
area_tb <- stilf_standard_date_events(data_tb = new_area_Sinop, month_year = "08", day_month = "15")

area_tb

#write.table(area_tb, "~/Desktop/Sinop_postprocess.csv", quote = FALSE, sep = ",", row.names = FALSE)

# plot maps input data
stilf_plot_maps_input(area_tb, EPSG_WGS84 = TRUE)


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
p3 <- "Soybean_Fallow2"
p4 <- "Soybean_NonComerc2"
p5 <- "Soybean_Fallow1"
p6 <- "Soybean_Comerc2"
p7 <- "Soybean_Comerc1"
p8 <- "Soybean_NonComerc1"
p9 <- "Pasture2"
p10 <- "Pasture1"
p11 <- "Soybean_Pasture"
p12 <- "NonComerc_Cotton"
p13 <- "Fallow_Cotton"
p14 <- "Soybean_Cotton"
p15 <- "Water"

# te = interval:
t1 <- stilf_interval("2000-08-15","2001-08-15")
t2 <- stilf_interval("2001-08-15","2016-08-15")

##
# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb
  
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  # create progress bar
  progress_bar <- txtProgressBar(min = 0, max = length(coord), style = 3)###
  
  for(x in 1:length(coord)){
    # x=333
    Sys.sleep(0.0)###
    
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if ((nrow(ev1.in1 <- stilf_predicate_occur_BigData(temp, p1, t1)) >= 1 | #forest in first interval
         
         nrow(ev2.in1 <- stilf_predicate_occur_BigData(temp, p2, t1)) >= 1 | # other classes in frist interval
         nrow(ev3.in1 <- stilf_predicate_occur_BigData(temp, p3, t1)) >= 1 |
         nrow(ev4.in1 <- stilf_predicate_occur_BigData(temp, p4, t1)) >= 1 |
         nrow(ev5.in1 <- stilf_predicate_occur_BigData(temp, p5, t1)) >= 1 |
         nrow(ev6.in1 <- stilf_predicate_occur_BigData(temp, p6, t1)) >= 1 |
         nrow(ev7.in1 <- stilf_predicate_occur_BigData(temp, p7, t1)) >= 1 |
         nrow(ev8.in1 <- stilf_predicate_occur_BigData(temp, p8, t1)) >= 1 |
         nrow(ev9.in1 <- stilf_predicate_occur_BigData(temp, p9, t1)) >= 1 |
         nrow(ev10.in1 <- stilf_predicate_occur_BigData(temp, p10, t1)) >= 1 |
         nrow(ev11.in1 <- stilf_predicate_occur_BigData(temp, p11, t1)) >= 1 |
         nrow(ev12.in1 <- stilf_predicate_occur_BigData(temp, p12, t1)) >= 1 |
         nrow(ev13.in1 <- stilf_predicate_occur_BigData(temp, p13, t1)) >= 1 |
         nrow(ev14.in1 <- stilf_predicate_occur_BigData(temp, p14, t1)) >= 1 |
         nrow(ev15.in1 <- stilf_predicate_occur_BigData(temp, p15, t1)) >= 1 
         
    ) 
    &
    (	  nrow(ev2.in2 <- stilf_predicate_occur_BigData(temp, p2, t2)) >= 1 | # other classes in second interval
        nrow(ev3.in2 <- stilf_predicate_occur_BigData(temp, p3, t2)) >= 1 |
        nrow(ev4.in2 <- stilf_predicate_occur_BigData(temp, p4, t2)) >= 1 |
        nrow(ev5.in2 <- stilf_predicate_occur_BigData(temp, p5, t2)) >= 1 |
        nrow(ev6.in2 <- stilf_predicate_occur_BigData(temp, p6, t2)) >= 1 |
        nrow(ev7.in2 <- stilf_predicate_occur_BigData(temp, p7, t2)) >= 1 |
        nrow(ev8.in2 <- stilf_predicate_occur_BigData(temp, p8, t2)) >= 1 |
        nrow(ev9.in2 <- stilf_predicate_occur_BigData(temp, p9, t2)) >= 1 |
        nrow(ev10.in2 <- stilf_predicate_occur_BigData(temp, p10, t2)) >= 1 |
        nrow(ev11.in2 <- stilf_predicate_occur_BigData(temp, p11, t2)) >= 1 |
        nrow(ev12.in2 <- stilf_predicate_occur_BigData(temp, p12, t2)) >= 1 |
        nrow(ev13.in2 <- stilf_predicate_occur_BigData(temp, p13, t2)) >= 1 |
        nrow(ev14.in2 <- stilf_predicate_occur_BigData(temp, p14, t2)) >= 1 |
        nrow(ev15.in2 <- stilf_predicate_occur_BigData(temp, p15, t2)) >= 1 
    ) 
    &
    nrow(ev1.in2 <- stilf_predicate_occur_BigData(temp, p1, t2)) >= 1 # if occur forest in second interval 
    & 
    # verify if forest discovered in interval 2 occur after start_date other classes. 
    # If TRUE, this forest is a secondary_vegetation  
    nrow(event3 <- ev1.in2[which(ev1.in2$start_date > min(
                                  ev2.in2$start_date, ev3.in2$start_date,
                                  ev4.in2$start_date, ev5.in2$start_date,
                                  ev6.in2$start_date, ev7.in2$start_date,
                                  ev8.in2$start_date, ev9.in2$start_date,
                                  ev10.in2$start_date, ev11.in2$start_date,
                                  ev12.in2$start_date, ev13.in2$start_date,
                                  ev14.in2$start_date, ev15.in2$start_date, na.rm=TRUE)),]) >=1 
    
    ){
      temp0 <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1, ev6.in1, ev7.in1,
                     ev8.in1, ev9.in1, ev10.in1, ev11.in1, ev12.in1, ev13.in1, ev14.in1, ev15.in1,
                     ev2.in2, ev3.in2, ev4.in2, ev5.in2, ev6.in2, ev7.in2,
                     ev8.in2, ev9.in2, ev10.in2, ev11.in2, ev12.in2, ev13.in2, ev14.in2, ev15.in2,
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
data_sec_veg_occurs <- QuestionOccurs(area_tb, p = c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15), t = c(t1,t2))
# options( warn = -1 ) # ignore warnings() -- To turn warnings back on, use options(warn=0)

rm(list = ls(pattern="^p[0-9]")) # remove classes
remove(t1,t2) # remove intervals


# remove duplicated rows
length(which(duplicated(data_sec_veg_occurs)))
#data_sec_veg_occurs <- data_sec_veg_occurs[!duplicated(data_sec_veg_occurs),]

stilf_plot_maps_input(area_tb, EPSG_WGS84 = TRUE) 
stilf_plot_maps_events(data_sec_veg_occurs, EPSG_WGS84 = TRUE) 



#############################
# Rules
#########

df_input <- area_tb

df_temp <- data_sec_veg_occurs

# remove first line of data.frame that contain the from == "2000-08-15" and change by other label
df_first_line <- df_temp[which(df_temp$start_date == "2000-08-15"),]
df_posProc <- df_temp[which(df_temp$start_date != "2000-08-15"),]

# replace Forest to Secondary-vegetation
df_posProc$label <- as.character(df_posProc$label)
df_posProc$label[df_posProc$label == "Forest"] <- "Secondary_vegetation"

# merge first line dataframe with pos-processing dataframe
df_pos <- dplyr::bind_rows(df_first_line,df_posProc)

# original data
df_temp2 <- df_input 

# temp3 dataframe with only id lines not in pos dataframe
df_temp3 <- df_temp2[!(df_temp2$id %in% df_pos$id),]

# merge df_input without id with renamed with lines without changes
df_input_new <- bind_rows(df_pos,df_temp3)
df_input_new <- data.frame(df_input_new[order(df_input_new$id),]) # order
#rownames(df_input_new) <- 1:nrow(df_input_new)

# return as tibble 
df_input_new <- tibble::as_tibble(df_input_new)

head(df_input_new)
# remove other
remove(df_temp,df_first_line,df_posProc,df_pos,df_temp2,df_temp3)

#############################

# stilf_toJSON(df_input_new, "~/Desktop/Sinop_postprocess.json") # foi salvo


##########
library(stilf)

stilf_starting_point()

file_json = "~/Desktop/Sinop_postprocess.json"

input_tb <- file_json %>% 
  stilf_fromJSON() 

input_tb

inter <- stilf_interval("2000-01-01","2016-12-31")

df1 <- stilf_predicate_occur(input_tb, "Soybean_NonComerc2", inter)

stilf_plot_maps_input(input_tb, EPSG_WGS84 = TRUE) 

stilf_plot_maps_events(df1, EPSG_WGS84 = TRUE) 

stilf_plot_sequence_events(df1)
stilf_plot_barplot_events(df1) 


###############################


#+++++++++++++++++++++++++++++ Area 1 - SBSR
## Question 3 - Which "Pasture" areas before have been replaced by soybean-millet in 2008?
# o = geo-objects, the own df_input data.frame

# Forest --> Pasture --> Cropping
df <- input_tb
df$"label2" <- df$label

# change label for legends
for (x in 1:nrow(df)){
  if (df$label2[x] == "Pasture1") {
    df$"label"[x] = "Pasture"
  }
  else if (df$label2[x] == "Pasture2") {
    df$"label"[x] = "Pasture"
  }
  else if (df$label2[x] == "Soybean_Fallow1") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Soybean_Fallow2") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Soybean_NonComerc1") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Soybean_NonComerc2") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Soybean_Comerc1") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Soybean_Comerc2") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Soybean_Pasture") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "NonComerc_Cotton") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Fallow_Cotton") {
    df$"label"[x] = "Cropping"
  }
  else if (df$label2[x] == "Soybean_Cotton") {
    df$"label"[x] = "Cropping"
  }
}

# p = properties of objects : 
p1 <- "Forest"

p2 <- "Pasture"
p3 <- "Cropping" 

# t = interval:
t1 <- stilf_interval("2000-08-15","2016-08-15")
#t2 <- stilf_interval("2011-08-15","2015-08-15")

# Test occur for many time series in a dataframe
#QuestionOccurs <- function(data_tb, p, t){
  
  #df <- data_tb <- input_tb

  
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  # create progress bar
  #progress_bar <- txtProgressBar(min = 0, max = length(coord), style = 3)###
  
#  for(x in 1:length(coord)){
    x=18
    #Sys.sleep(0.0)###
    
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    for (i in 1:nrow(temp)) {
      i=1
        if (nrow(ev1 <- stilf_predicate_occur_BigData(temp[i,], p1, t1)) >= 1 &
            (
            nrow(ev2 <- stilf_predicate_occur_BigData(temp[i,], p2, t1)) >= 1 |
            nrow(ev3 <- stilf_predicate_occur_BigData(temp, p3, t1)) >= 1
            ) &
            nrow(ev4 <- stilf_predicate_occur_BigData(temp, p4, t1)) >= 1
            & 
            (
            isTRUE(stilf_relation_meets(stilf_interval(head(ev1$start_date,1),tail(ev1$end_date,1)),
                                        stilf_interval(head(ev2$start_date,1),tail(ev2$end_date,1)))) |
            isTRUE(stilf_relation_meets(stilf_interval(head(ev1$start_date,1),tail(ev1$end_date,1)),
                                        stilf_interval(head(ev3$start_date,1),tail(ev3$end_date,1)))) 
            ) & 
            (
            isTRUE(stilf_relation_meets(stilf_interval(head(ev2$start_date,1),tail(ev2$end_date,1)),
                                        stilf_interval(head(ev4$start_date,1),tail(ev4$end_date,1)))) |
            isTRUE(stilf_relation_meets(stilf_interval(head(ev3$start_date,1),tail(ev3$end_date,1)),
                                        stilf_interval(head(ev4$start_date,1),tail(ev4$end_date,1))))
            )
        ){
          temp0 <- rbind(ev1, ev2, ev3, ev4)
        } else {
          temp0 <- NULL
        }
       # output_df <- dplyr::bind_rows(output_df,temp0)
  }
 # return(output_df)
#}

# verify using function
pasture_soybean <- QuestionOccurs(input_tb, p = c(p9,p10), t = c(t1))

rm(list = ls(pattern="^p[0-9]")) # remove classes
remove(t1) # remove intervals

# remove duplicated rows
length(which(duplicated(pasture_soybean)))
#pasture_soybean <- pasture_soybean[!duplicated(pasture_soybean),]

stilf_plot_maps_input(input_tb, EPSG_WGS84 = TRUE) 
stilf_plot_maps_events(pasture_soybean, EPSG_WGS84 = TRUE) 

#############################

aux <- df[which(df$index == 18),]

# verify if patterns exxist
grepl(transiction, aux$label)

"Forest --> Pasture --> Cropping" 
coord <- unique(df$index)
output_df <- aux <- df[FALSE,]

# create progress bar
#progress_bar <- txtProgressBar(min = 0, max = length(coord), style = 3)###

#  for(x in 1:length(coord)){
x=18
#Sys.sleep(0.0)###

sentenca <- df[which(as.character(df$index) == coord[x]),]

# 
iniciar <- function(){
  cont<-1
  q0(cont)
}

q0 <- function(cont){
  if(cont <= nrow(sentenca)){
    if(sentenca$label[cont] == 'Forest' && cont != nrow(sentenca)){
      cont<-cont+1
      aux <- rbind(aux, sentenca[count,])
      q1(cont)
    } else if(sentenca$label[cont] == 'Forest' && cont == nrow(sentenca)){
      break;
      qerror()
    } else if(sentenca$label[cont] == 'Pature' && cont != nrow(sentenca)){
      cont<-cont+1
      q3(cont)
    } else if(sentenca$label[cont] == 'Pature' && cont == nrow(sentenca)){
      break
      qerror()    
    } else if(sentenca$label[cont] == 'Cropping' && cont != nrow(sentenca)){
      cont<-cont+1
      q3(cont)
    } else if(sentenca$label[cont] == 'Cropping' && cont == nrow(sentenca)){
      qf()    
    } else
      qerro()
  } else 
    qerro()
}

q1 <- function(cont){
  if(cont <= nrow(sentenca)){
    if(palavra[cont]=='Forest' && cont != nrow(sentenca)){
      cont<-cont+1
      q0(cont)
    } else if(palavra[cont] == 'Forest' && cont == nrow(sentenca)){
      qf()
    } else if(palavra[cont] == 'Cropping' && cont != nrow(sentenca)){
      cont<-cont+1
      q4(cont)
    } else if(palavra[cont] == 'Cropping' && cont == nrow(sentenca)){
      qf()    
    } else if(palavra[cont] == 's' && cont != nrow(sentenca)){
      cont<-cont+1
      q4(cont)
    } else if(palavra[cont] == 's' && cont == nrow(sentenca)){
      qf()    
    } else
      qerro()
  } else 
    qerro()
}

q2 <- function(cont){
  if(cont <= nrow(sentenca)){
    if(palavra[cont]=='Pature' && cont != nrow(sentenca)){
      cont<-cont+1
      q1(cont)
    } else if(palavra[cont] == 'Pature' && cont == nrow(sentenca)){
      qf()
    } else if(palavra[cont] == 'Cropping' && cont != nrow(sentenca)){
      cont<-cont+1
      q4(cont)
    } else if(palavra[cont] == 'Cropping' && cont == nrow(sentenca)){
      qf()    
    } else if(palavra[cont] == 's' && cont != nrow(sentenca)){
      cont<-cont+1
      q4(cont)
    } else if(palavra[cont] == 's' && cont == nrow(sentenca)){
      qf()    
    } else
      qerro()
  } else 
    qerro()
}

q3 <- function(cont){
  if(cont <= nrow(sentenca)){
    if(palavra[cont]=='Pature' && cont != nrow(sentenca)){
      cont<-cont+1
      q3(cont)
    } else if(palavra[cont] == 'Pature' && cont == nrow(sentenca)){
      qf()  
    } else if(palavra[cont] == 'Cropping' && cont != nrow(sentenca)){
      cont<-cont+1
      q4(cont)
    } else if(palavra[cont] == 'Cropping' && cont == nrow(sentenca)){
      qf()
    } else if(palavra[cont] == 's' && cont != nrow(sentenca)){
      cont<-cont+1
      q4(cont)
    } else if(palavra[cont] == 's' && cont == nrow(sentenca)){
      qf()
    } else 
      qerro()
  } else 
    qerro()
}

qf <- function(){
  print('Transiction OK!!!')
}

qerro <- function(){
  print('Transiction ERROR!!!')
}

# sentenca <- "ffffff"
# palavra <- strsplit(sentenca, "")[[1]]
sentenca <- aux$label
palavra <- sentenca

cont <- 0
iniciar()  





library(sits)

tes <- sits_getdata("~/Dropbox/TWDTWAmazoniaCerrado/Amostras/samples_Damien_Ieda_11classes_6bands.json")
tes




















