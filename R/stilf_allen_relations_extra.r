#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with extra Allen's relationships                 ##
##                                                             ##
##                                             2017-06-23      ##
##                                                             ##
##  J. F. Allen.  Towards a general theory of action and       ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


# ALLEN'S INTERVAL ALGEBRA
# Thirteen basic relation
#
# before        (end_I < start_J) -> precedes
# after         (start_I > end_J) -> preceded by
# meets         (end_I == start_J)
# met by        (end_J == start_I)
# overlaps      (start_I < start_J) & (end_I > start_J) & (end_I < end_J)
# overlapped by (end_I > start_J) & (start_I < end_J) & (end_I > end_J)
# starts        (start_I == start_J) & (end_I < end_J)
# started by    (start_I == start_J) & (end_I > end_J)
# during        (start_I > start_J) & (end_I < end_J))
# contains      (start_I < start_J) & (end_I > end_J)
# finishes      (start_I > start_J) & (end_I == end_J)
# finished by   (start_I < start_J) & (end_I == end_J)
# equals        (start_I == start_J) & (end_I == end_J)

# Derivates relations
# in            (during(first_interval, second_interval) | starts(first_interval, second_interval) 
#                   | finishes(first_interval, second_interval))
# follows       (meets(first_interval, second_interval) | before(first_interval, second_interval))
# precedes      (met_by(first_interval, second_interval) | after(first_interval, second_interval))



#' @title Allen Relation In
#' @name stilf_relation_in
#' @aliases stilf_relation_in
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an extra Allen's interval relation to classified time series data. And return a logical value if an interval is TRUE or FALSE
#' 
#' @usage stilf_relation_in(first_interval , second_interval)
#'  
#' @param first_interval  stilf_interval. An interval between two dates.
#' @param second_interval stilf_interval. An interval between two dates.
#' 
#' @keywords datasets
#' @return Logical value if interval are TRUE or FALSE
#' @importFrom lubridate is.interval int_standardize
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # create some examples of intervals
#' time7 <- stilf_interval("2011-08-01","2011-09-15")
#' time8 <- stilf_interval("2011-08-15","2011-08-29")
#' 
#' # Two interval I and J, with:
#' # I <- ("2011-08-15","2011-08-29")
#' # J <- ("2011-09-01","2011-10-01")
#' # end_I equals to the final value of the interval, I == "2011-08-29"
#' # start_I equals to the begin value of the interval, I == "2011-08-15"
#' # end_J equals to the final value of the interval, J == "2011-10-01"
#' # start_J equals to the begin value of the interval, J == "2011-09-01"
#' 
#' # Apply a relation 'in'  (during(first_interval, second_interval) | 
#' # starts(first_interval, second_interval) | 
#' # finishes(first_interval, second_interval))
#' stilf_relation_in(time8,time7)
#' 
#'            
#'}
#'

# 14. The 'stilf_relation_in' relation = stilf_relation_during v stilf_relation_starts v stilf_relation_finishes
stilf_relation_in <- function(first_interval, second_interval){
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  stilf_relation_during(first_interval, second_interval) | 
    stilf_relation_starts(first_interval, second_interval) | 
    stilf_relation_finishes(first_interval, second_interval)
}


#' @title Allen Relation Follows
#' @name stilf_relation_follows
#' @aliases stilf_relation_follows
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an extra Allen's interval relation to classified time series data. And return a logical value if an interval is TRUE or FALSE
#' 
#' @usage stilf_relation_follows(first_interval , second_interval)
#'  
#' @param first_interval  stilf_interval. An interval between two dates.
#' @param second_interval stilf_interval. An interval between two dates.
#' 
#' @keywords datasets
#' @return Logical value if interval are TRUE or FALSE
#' @importFrom lubridate is.interval int_standardize
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # create some examples of intervals
#' time1 <- stilf_interval("2011-09-01","2011-10-01")
#' time3 <- stilf_interval("2011-10-01","2011-11-01")
#' 
#' # Apply a relation 'follows' (meets(first_interval, second_interval) |
#' # before(first_interval, second_interval))
#' stilf_relation_follows(time1,time3)
#' 
#'}
#'

# 15. The 'stilf_relation_follows' relation = stilf_relation_meets v stilf_relation_before
stilf_relation_follows <- function(first_interval, second_interval){
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  stilf_relation_before(first_interval, second_interval) | 
    stilf_relation_meets(first_interval, second_interval)
}


#' @title Allen Relation Precedes
#' @name stilf_relation_precedes
#' @aliases stilf_relation_precedes
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an extra Allen's interval relation to classified time series data. And return a logical value if an interval is TRUE or FALSE
#' 
#' @usage stilf_relation_precedes(first_interval , second_interval)
#'  
#' @param first_interval  stilf_interval. An interval between two dates.
#' @param second_interval stilf_interval. An interval between two dates.
#' 
#' @keywords datasets
#' @return Logical value if interval are TRUE or FALSE
#' @importFrom lubridate is.interval int_standardize
#' @export
#'
#' @examples \dontrun{
#' 
#' library(stilf)
#' 
#' stilf_starting_point()
#' 
#' # create some examples of intervals
#' time1 <- stilf_interval("2011-09-01","2011-10-01")
#' time3 <- stilf_interval("2011-10-01","2011-11-01")
#' 
#' # Apply a relation 'precedes' (met_by(first_interval, second_interval) |
#' # after(first_interval, second_interval))
#' stilf_relation_precedes(time1,time3)
#'          
#'            
#'}
#'

# Antonyms of following
# 16. The 'stilf_relation_precedes' relation = stilf_relation_met_by || stilf_relation_after
stilf_relation_precedes <- function(first_interval, second_interval){
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  stilf_relation_after(first_interval, second_interval) | 
    stilf_relation_met_by(first_interval, second_interval)
}


