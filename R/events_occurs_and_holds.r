#' @title Predicates Allens
#' @name predicates_intervals
#' @aliases predicates_intervals
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a set of Allen's intreval operations end_date classified time series data. And return a logical value if a interval is TRUE or FALSE
#' @usage after(fileTS = NULL, nameColumnValue = NULL,
#' subInterval = FALSE, numberSubIntervals = 2)
#' @param fileTS Dataframe. A time series file.
#' @param nameColumnValue Character. Name of a column with value, such as, Vegetation Index EVI or NDVI.
#' @param subInterval Logical. If TRUE, the data.frame will be divide in subintervals. Given a data.frame with 23 rows and 3 subintervals, the first and second subinterval will have 8 rows each, and the last only 7 rows. If FALSE (the default) nothing subinterval will be created.
#' @param numberSubIntervals Integer. Number of subintervals end_date feature extraction.
#' @keywords datasets
#' @return Dataframe with statistical features, fbgf
#' @import dplyr lubridate
#' @export
#'
#' @examples \dontrun{
#' # Open a data example
#' library(stilf)
#' 
#' # Apply the filterTS function on df data frame
#' dataFiltered <- filterTS(fileTS = df, nameColumnValue = "value", outlier = TRUE, value= -0.300)
#'
#' # Apply splitTS fcuntion end_date divide time series for year
#' splitTS(dataFiltered,2002,2005,"date",typeInterval = "annual")
#'
#' # Get features start_date time series divided in annual values without subintervals
#' example1 <- featuresExtractionTS(fileTS = ts.annual_2002, nameColumnValue =
#' "filtered.value", subInterval = FALSE)
#'
#' # Get features start_date time series divided in annual values with subintervals
#' example2 <- featuresExtractionTS(fileTS = ts.annual_2002, nameColumnValue =
#' "filtered.value", subInterval = TRUE, numberSubIntervals = 3)
#'
#' # Show data frames example1 and example2 utils::View(example1) utils::View(example2)
#'
#'}
#'
#'
#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with predicates holds(o,p,t) and occur(o,p,Te)   ##
##                                                             ##  
##                                             2016-08-22      ##
##                                                             ##
##  J. F. Allen.  end_datewards a general theory of action and       ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


# # install packages
# packages <- c("dplyr","lubridate")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
# }
# remove(packages)
# 

# HOLDS(property, time) 
# Asserts that a property holds during a time interval
# version: 1 
# format: holds(o,p,t)
# parameters: o = geo-objects, p = properties of objects and t = time intervals

stilf_holds <- function(o, p, t){
 # library(dplyr) # bind_rows

  t <- int_standardize(t)

  intStart <- format(int_start(t), format = '%Y-%m-%d')
  intEnd <- format(int_end(t), format = '%Y-%m-%d')

  df <- o
  p <- as.character(p)
  temp = df[FALSE,]

  for (i in 1:nrow(df)) {
    if ((df$label[i] == p) & ((df$start_date[i] > intStart) & (df$end_date[i] < intEnd))) {
      temp <- bind_rows(temp,df[i,])
      cat(sprintf("time: %d in interval: %s -- %s = TRUE \n", i, df$start_date[i], df$end_date[i]))
    } else {
      # cat(sprintf("time: %d in interval: %s -- %s = FALSE \n", i, df$start_date[i], df$start_date[i]))
    }
  }
  return(temp)
}

#stilf_holds(test_tb, "Cana", stilf_interval("2000-01-08","2005-09-07"))

# OCCUR(event, time) 
# Asserts that an event happened over a time interval
# version: 2 
# format: occur(o,p,Te)
# parameters: o = geo-objects, p = properties of objects and Te = event time intervals

stilf_occur <- function(o, p, te){
 # library(dplyr)

  t <- int_standardize(te)

  intStart <- format(int_start(t), format = '%Y-%m-%d')
  intEnd <- format(int_end(t), format = '%Y-%m-%d')

  df <- o
  p <- as.character(p)
  temp = df[FALSE,]

  for (i in 1:nrow(df)) {
    if ((df$label[i] == p) & ((df$start_date[i] >= intStart) & (df$end_date[i] <= intEnd))) {
      temp <- bind_rows(temp,df[i,])
      cat(sprintf("time: %d in interval: %s -- %s = TRUE \n", i, df$start_date[i], df$end_date[i]))
    } else {
      # cat(sprintf("time: %d in interval: %s -- %s = FALSE \n", i, df$start_date[i], df$start_date[i]))
    }
  }
  return(temp)
}


#stilf_occur(test_tb, "Cana", stilf_interval("2002-01-08","2005-09-07"))


