#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to plot events as maps and sequences             ##
##                                                             ##  
##                                             2016-08-22      ##
##                                                             ##
#################################################################


# install packages
packages <- c("dplyr","sp","rgdal","raster")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
}
remove(packages)


############
# RAW Dataframe
############

#************************
# 1. Map for df_input dataframe, raw file
#************************

# Test holds for many time series in a dataframe

createPoints <- function(df, LongLat){ 

  # load libraries
  library(dplyr)
  library(sp)
  library(rgdal)
  library(raster)
  
  dfFig <- df # 
  #dfFig <- df_input # 
  #head(dfFig)
  
  ## before, remove sequence of other maps
  # list_pts <- grep(x= ls(pos=1), pattern="pts_", value=TRUE)
  # list_pts
  list_p <- function() ls(envir= parent.frame(), pattern="pts_")
  list_pts <- list_p()
  remove(list=list_pts)
  
  # starts here
  dates <- unique(format(as.Date(dfFig$to), format = '%Y'))
  indexLong <- which(colnames(dfFig) == "longitude")
  indexLat <- which(colnames(dfFig) == "latitude")
  indexLabel <- which(colnames(dfFig) == "label")
  
  for(x in 1:length(dates)){
    #x=1
    map <- filter(dfFig, grepl(dates[x], as.character(dfFig$to), fixed = TRUE))
    pts <- map[c(indexLong:indexLat,indexLabel)] # long, lat and class
    colnames(pts) <- c('x', 'y', 'z')
  
    if (LongLat == TRUE) {
      # converse to sinusoidal projection in case values in Longitude and Latitude
      d <- data.frame(x=pts$x, y=pts$y, z=pts$z, w=dates[x])
      coordinates(d) <- cbind(pts$x, pts$y)  
      proj4string(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      CRS.new <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
      d <- spTransform(d, CRS.new)
      
    } else if (LongLat == FALSE) {
        # use in case data from SciDB col and row
        d <- data.frame(x=pts$x, y=pts$y, z=pts$z, w=dates[x])
        coordinates(d) <- cbind(pts$x, pts$y)  
    } else {
        stop("FALSE/TRUE")
    }
    #plot(d)
    
    pts1 <- as.data.frame(d)
    colnames(pts1) <- c('x1', 'y1', 'z','w', 'x', 'y')
    pts1 <- data.frame(pts1$x,pts1$y,pts1$z,pts1$w,pts1$x1,pts1$y1)
    names(pts1)[1:6] = c('x', 'y', 'z','w','x1', 'y1')
    ptss <- sprintf("pts_%s",dates[x]) # para completo
    assign(ptss,pts1, envir = parent.frame())
    assign(ptss,pts1, envir = .GlobalEnv)
  }

}  
  
  #************************
  # 2. For df_input dataframe, generate a map
  #************************

plotMapsInput <- function(df, LongLat = FALSE){ #, path_output, width_png, height_png){
  
  createPoints(df, LongLat)
  
  ## Points image
  #list_a <- grep(x= ls(pos=1), pattern="pts_", value=TRUE)
  #list_a
  list_p <- function() ls(envir= parent.frame(), pattern="pts_")
  list_a <- list_p()
  a <- rbind.data.frame(do.call(rbind,mget(list_a)))
  rownames(a) <- NULL
  a <- data.frame(a) %>% filter(w != "NA")
  a$x <- as.integer(a$x)
  a$y <- as.integer(a$y)
  a$w <- as.factor(a$w)
  a$z <- as.factor(a$z)
  data.m <- a
  
  assign("data.m",data.m,envir= parent.frame())
  assign("data.m",data.m, envir = .GlobalEnv)
  # png(sprintf("%sAllMaps.png",path_output), width = width_png, height = height_png, res=300) # ESSA
  # pdf(sprintf("%sAllMaps.pdf",path_output), width = 7.5, height = 5, pointsize=16)

    # plot images all years
  library(ggplot2)
  g <- ggplot(data.m, aes(x, y)) +
        geom_raster(aes_string(fill=data.m$"z")) +
        scale_y_continuous(expand = c(0, 0), breaks = NULL) +
        scale_x_continuous(expand = c(0, 0), breaks = NULL) +
        facet_wrap("w") +
        coord_fixed(ratio = 1) + 
        #coord_fixed(ratio = 1/cos(mean(x)*pi/180)) +
        theme(legend.position = "bottom", strip.text = element_text(size=10)) +
        xlab("") +
        ylab("") +
        scale_fill_brewer(name="Legend:", palette= "Set3")
        #scale_fill_manual(name="Legend:", values=c("lightgoldenrodyellow", "lightgreen","steelblue1","lightpink","indianred2","tomato"))
        #scale_fill_manual(name="Legend:", values=c("tan1", "chartreuse2","cornsilk","violetred2","forestgreen","tomato"))  #values = unique(data.m$z)) +
 
 # if (dev.interactive()) 
#    dev.new()
  g
  
  # dev.off()    
    
}

# path_output = "~/Desktop/Eventos_02_MT/figures/"
# width_png = 2900
# height_png = 2400
# plotMapsInput(df_input)  





############
# EVENTS
############

#************************
# 1. Map for df_output... create a map for each result - file with result of question
#************************

createPointsEvents <- function(df, LongLat){ 
  
  # load libraries
  library(dplyr)
  library(sp)
  library(rgdal)
  library(raster)
  
  dfFig_E <- df
  #dfFig_E <- df_outputOccurs1
  #head(dfFig_E)
  
  ## before, remove sequence of other maps
  # list_ptsE <- grep(x= ls(pos=1), pattern="ptsE_", value=TRUE)
  # list_ptsE
  list_pE <- function() ls(envir= parent.frame(), pattern="ptsE_")
  list_ptsE <- list_pE()
  remove(list=list_ptsE)
  
  # starts here
  dates <- unique(format(as.Date(dfFig_E$to), format = '%Y'))
  indexLong <- which(colnames(dfFig_E) == "longitude")
  indexLat <- which(colnames(dfFig_E) == "latitude")
  indexLabel <- which(colnames(dfFig_E) == "label")
  
  for(x in 1:length(dates)){
    #x=1
    map <- filter(dfFig_E, grepl(dates[x], as.character(dfFig_E$to), fixed = TRUE))
    pts <- map[c(indexLong:indexLat,indexLabel)] # long, lat and class
    colnames(pts) <- c('x', 'y', 'z')
    
    if (LongLat == TRUE) {
      # converse to sinusoidal projection in case values in Longitude and Latitude
      d <- data.frame(x=pts$x, y=pts$y, z=pts$z, w=dates[x])
      coordinates(d) <- cbind(pts$x, pts$y)  
      proj4string(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      CRS.new <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
      d <- spTransform(d, CRS.new)
      
    } else if (LongLat == FALSE) {
      # use in case data from SciDB col and row
      d <- data.frame(x=pts$x, y=pts$y, z=pts$z, w=dates[x])
      coordinates(d) <- cbind(pts$x, pts$y)  
    } else {
      stop("FALSE/TRUE")
    }
    #plot(d)
    
    pts1 <- as.data.frame(d)
    colnames(pts1) <- c('x1', 'y1', 'z','w', 'x', 'y')
    pts1 <- data.frame(pts1$x,pts1$y,pts1$z,pts1$w,pts1$x1,pts1$y1)
    names(pts1)[1:6] = c('x', 'y', 'z','w','x1', 'y1')
    ptss <- sprintf("ptsE_%s",dates[x]) # para eventos
    assign(ptss,pts1, envir = parent.frame())
    assign(ptss,pts1, envir = .GlobalEnv)
  }
  
}  
  
  
  #************************
  # 2. For df_output...  dataframe, generate a map with events
  #************************
plotMapsOutputEvents <- function(df, LongLat = FALSE, sizeSquare=1){ #, path_output, question, width_png, height_png){
  
  createPointsEvents(df, LongLat)
  
  ## Points event
  #list_b <- grep(x= ls(pos=1), pattern="ptsE_", value=TRUE)
  #list_b
  list_pE <- function() ls(envir= parent.frame(), pattern="ptsE_")
  list_b <- list_pE()
  b <- rbind.data.frame(do.call(rbind,mget(list_b)))
  rownames(b) <- NULL
  b <- data.frame(b) %>% filter(w != "NA")
  b$x <- as.integer(b$x)
  b$y <- as.integer(b$y)
  b$w <- as.factor(b$w)
  b$z <- as.factor(b$z)
  data.e <- b
  
  assign("data.e",data.e,envir= parent.frame())
  assign("data.e",data.e, envir = .GlobalEnv)
  
  #png(sprintf("%sMap_Question_%s.png",path_output,question), width=width_png, height=height_png, res=300) # ESSA
  #pdf(sprintf("%sMap_Question_%s.png",path_output,question), width = 7.5, height = 5, pointsize=16)
  
  # plot only events
  library(ggplot2)
  g <- ggplot() +
        geom_raster(data=data.m, aes(x, y, fill=data.m$"z")) +
        geom_point(data=data.e, aes(x=x, y=y), shape=0, colour = "black" ) + #, size=sizeSquare) + #  size=1.4
        scale_y_continuous(expand = c(0, 0), breaks = NULL) +
        scale_x_continuous(expand = c(0, 0), breaks = NULL) +
        facet_wrap("w") +
        coord_fixed(ratio = 1) + 
        #coord_fixed(ratio = 1/cos(mean(x)*pi/180)) +
        theme(legend.position = "bottom", strip.text = element_text(size=10)) +
        xlab("") +
        ylab("") +
        scale_fill_brewer(name="Legend:", palette= "Set3")
        #scale_fill_manual(name="Legend:", values=c("lightgoldenrodyellow", "lightgreen","steelblue1","lightpink","indianred2","tomato"))
        #scale_fill_manual(name="Legend:", values=c("tan1", "chartreuse2","cornsilk","violetred2","forestgreen","tomato"))
        #scale_fill_manual(name="Legend:", values=c("cornsilk", "palegreen2","violetred2","tan1","dodgerblue1","tomato"))
  
#  if (dev.interactive()) 
#   dev.new()
  g

 #dev.off()
    
}

# path_output = "~/Desktop/Eventos_02_MT/figures/"
# width_png = 2900
# height_png = 2400
# question <- "01" # will be used as label to map .png 
# plotMapsOutputEvents(df_outputOccurs1, sizeSquare=1.4) # path_output, question, width_png, height_png){



############
# PLOT OF TIME SERIES AS SEQUENCE OF LINES OVER TIME - ONLY EVENT FILE output...
############

plotSequenceEvents <- function(df, dateStart="2000-01-01", dateEnd="2016-12-31"){ #, path_output, question, width_png, width_png){
    
  # alter this file name
  dfSeq <- df
  
  #png(sprintf("%sSeq_Question_%s.png",path_output,question), width=width_png, height=width_png, res=300)
  
  dfSeq$from <- as.Date(dfSeq$from, format = '%Y-%m-%d')
  dfSeq$to <- as.Date(dfSeq$to, format = '%Y-%m-%d')
  
  library(ggplot2)
  data <- as.data.frame(dfSeq) # data from package datasets
  data$Category <- as.character(dfSeq$ID) # this makes things simpler later
  
  g <- ggplot(data, aes(y = Category)) +
        labs(x = "Years", y = "Time series set") +
        theme_bw()+
        geom_segment(aes(x = data$"from", y = Category,
                         xend = data$"to", yend = Category,
                         color = data$"label"), size = 1.25) +
        
        geom_point(aes(x = data$"from", color =  data$"label"), size = 3, shape = 19) +
        geom_point(aes(x = data$"to", color = data$"label"), size = 3, shape = 19) +
        
        # alter this lines with interval, if you wish
        #geom_vline(xintercept = c(as.numeric(as.Date("2001-08-15")),as.numeric(as.Date("2002-08-15"))), linetype="dotted", colour="black", size = 0.8) + # Q1
        #geom_vline(xintercept = c(as.numeric(as.Date("2006-08-15"))), linetype="dotted", colour="black", size = 0.8) + # Q2
        
        # define time period
        scale_x_date(limits=as.Date(c(dateStart, dateEnd))) +
        #scale_color_discrete(name = "Legend:") +
        scale_color_brewer(name="Legend:", palette= "Set3") +
        theme(legend.position = "bottom", legend.text=element_text(size=10),legend.key = element_blank())
        #axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
 # if (dev.interactive()) 
#    dev.new()
  g
 
  #dev.off()
  
}

# path_output = "~/Desktop/Eventos_02_MT/figures/"
# width_png = 2900
# height_png = 2400
# question <- "01" 
# plotSequenceEvents(df_outputOccurs1, dateStart="2000-01-01", dateEnd="2016-12-31") # path_output, question, width_png, height_png){



############
# PLOT OF BARPLOT OVER TIME - ONLY EVENT FILE output...
############

plotBarplotEvents <- function(df){ 
  
  dfSeq <- df
  
  #head(data.e)
  #data.frame(table(data.e$w, data.e$z))
  dfBar <- data.frame(table(df$w, df$z))
  head(dfBar)
  
  #png(filename="~/Desktop/plotBarQ5.png", width=2400, height=1500, res=300)
  
  library(ggplot2)
  g <- ggplot(dfBar,aes(x=Var1, y=(Freq*(250*250))/(1000*1000), fill=Var2))+
        geom_bar(width = 0.7, stat="identity")+ #, position=position_dodge())+
        theme_bw()+
        ylab(expression("Area km"^{2}))+
        xlab("Years")+
        scale_fill_brewer(name="Legend:", palette= "Set3") +
        theme(legend.position = "bottom", legend.text=element_text(size=10), legend.key = element_blank())
        #theme(legend.position = "bottom", legend.text=element_text(size=10),
        #     axis.text.x = element_text(size=10), axis.text.y = element_text(size=10)) 
        #     text = element_text(size=10))
  
 # if (dev.interactive()) 
#    dev.new()
  g
  
  #dev.off()

}

# barplot
# plotBarplotEvents(data.e)


