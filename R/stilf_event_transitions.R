#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##       R script to discover event transitions                ##
##                                                             ##
##                                             2017-03-18      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title STILF Event Transitions
#' @name stilf_event_transitions
#' @aliases stilf_event_transitions
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a stilf_event_transitions, a set of event transitions. 
#' And return a tibble with events discovered
#'
#' @usage stilf_event_transitions(data_tb , properties)
#' 
#' @param data_tb        Tibble. A data frame with input values.
#' @param properties     Character. A vector with ate least two strings, 
#' for example, c("Forest","Cropping").
#' 
#' @keywords datasets
#' @return Tibble with event transition 
#' @import dplyr ensurer
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # open a JSON file with data classified
#' input_tb <- "./inst/postprocess_Sinop.json" %>% 
#'   stilf_fromJSON() 
#' 
#' # if necessary, create a new column to rpeserve the old label
#' df <- input_tb
#' df$"label2" <- df$label
#' 
#' # change label for 
#' for (x in 1:nrow(df)){
#'   if (df$label2[x] == "Savanna") {
#'     df$"label"[x] = "Cerrado"
#'   }
#'   else if (df$label2[x] %in% c("Pasture1","Pasture2")) {
#'     df$"label"[x] = "Pasture"
#'   }
#'   else if (df$label2[x] %in% c("Soybean_Fallow1", "Soybean_Fallow2", 
#'                                "Soybean_NonComerc1", "Soybean_NonComerc2", 
#'                                "Soybean_Comerc1", "Soybean_Comerc2", 
#'                                "Soybean_Pasture", "NonComerc_Cotton", 
#'                                "Fallow_Cotton", "Soybean_Cotton")) {
#'     df$"label"[x] = "Cropping"
#'   }
#' }
#' 
#' df
#' 
#' # create a tibble with the same column names 
#' output.tb <- df[FALSE,]
#' coord <- unique(df$index)
#' 
#' # Forest --> Pasture --> Cropping - Type of transition
#' transition_string <- c("Forest", "Pasture", "Cropping")
#' 
#' # Apply over all input data
#' for(x in 1:length(coord)){
#'   temp.tb <- df[which(as.character(df$index) == coord[x]),]
#'   temp_final.tb <- stilf_event_transitions(temp.tb, properties = transition_string)
#'   output.tb <- bind_rows(output.tb, temp_final.tb)
#' }
#' output.tb
#' 
#' # plot transitions discovered
#' stilf_plot_maps_input(df, EPSG_WGS84 = TRUE)
#' stilf_plot_maps_events(output.tb, EPSG_WGS84 = TRUE)
#' stilf_plot_sequence_events(output.tb)
#' 
#'
#'

# function to apply event transtion
stilf_event_transitions <- function(data_tb = NULL, properties = NULL){
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, must be defined!\n")
  ensurer::ensure_that(properties, !is.null(properties) & (length(properties)>=2 & length(properties)<=5) & is.character(properties), 
                       err_desc = "Properties must be character type and have at least two strings and maximum five.")
  
  # create a set of variables with each string from vector stored 
  for(i in 1:length(properties)){
    assign(paste0("properties_",i), as.character(properties[i]))
  }
  
  # start transition
  start_transition <- function(count, interval.tb, aux.tb){
    count <- 1
    aux.tb <- state_0(count, interval.tb, aux.tb)
    aux.tb
  }
  
  # state 0
  state_0 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(all(interval.tb$label[count] != properties) && count != nrow(interval.tb)){
        count <- count+1
        aux.tb <- state_0(count, interval.tb, aux.tb)
      } else if(interval.tb$label[count] == properties_1 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_1(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 1, to first transition (or string)
  state_1 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_1 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_1(count, interval.tb, aux.tb)
      } else if(interval.tb$label[count] == properties_2 && count <= nrow(interval.tb)){
        aux.tb <- state_2(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 2, to second transition (or string)
  state_2 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_2 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_2(count, interval.tb, aux.tb)
      } else if (all(exists("properties_3") == TRUE && interval.tb$label[count] == properties_3 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_3(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 3, is applied if the third string exists
  state_3 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_3 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_3(count, interval.tb, aux.tb)
      } else if (all(exists("properties_4") == TRUE && interval.tb$label[count] == properties_4 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_4(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 4, is applied if the fourth string exist
  state_4 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_4 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_4(count, interval.tb, aux.tb)
      } else if (all(exists("properties_5") == TRUE && interval.tb$label[count] == properties_5 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_5(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 5, is applied if the fifth string exist
  state_5 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_5 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_5(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # case transition don't exists, compute the tibble auxiliar
  state_error <- function(aux.tb){
    aux.tb
  }
  
  # create a new tibble with the same column names from input tibble
  aux.tb <- output.tb <- data_tb[FALSE,]
  interval.tb <- data_tb
  count <- 0
  
  # start the event transition
  aux.tb <- start_transition(count, interval.tb, aux.tb)
  
  # create a stilf interval with information from start and end date from input tibble
  interval <- stilf_interval(head(data_tb$start_date,1),tail(data_tb$end_date,1))
  
  logical <- NULL
  
  # verify if all properties are TRUE
  if(nrow(aux.tb)>= 1) { # if tibble have more than one row
    
    # verify if the tibble created by event transition has all properties defined (between 2 and 5 strings) 
    for(i in 1:length(properties)){
      if(nrow(stilf_predicate_occur_BigData(aux.tb, mget(paste0("properties_", i)), interval))>=1){
        logical <- cbind(logical,"TRUE")
      } else{
        logical <- cbind(logical,"FALSE")
      }
    }
    
    # ensure that tibble output will contain only transtions with strings of input
    # if all events that occur are TRUE, the tibble output will receive the tibble aux.tb
    if(all(logical == TRUE)) 
      output.tb <- dplyr::bind_rows(output.tb, aux.tb)
    else {
      aux.tb <- NULL
      output.tb <- dplyr::bind_rows(output.tb, aux.tb)
    }
  } else {
    aux.tb <- NULL
    output.tb <- dplyr::bind_rows(output.tb, aux.tb)
  }
  
  output.tb
  
}      

