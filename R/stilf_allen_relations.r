#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with thirteen Allen's relationships              ##
##                                                             ##
##                                             2017-02-26      ##
##                                                             ##
##  J. F. Allen.  Towards a general theory of action and       ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################



#' @title Allens Intervals
#' @name stilf_interval
#' @aliases stilf_interval
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a stilf_interval, a input pattern format used by Allen's relations. And return a value of interval
#' @usage stilf_interval(first_date , second_date)
#' 
#' @param first_date      Date. An date value like '2012-03-02'.
#' @param second_date     Date. An date value like '2013-03-02'.
#' 
#' @keywords datasets
#' @return Interval value with two dates 
#' @import lubridate dplyr
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
#' time2 <- stilf_interval("2011-09-15","2011-11-01")
#' time3 <- stilf_interval("2011-10-01","2011-11-01")
#' time4 <- stilf_interval("2011-10-01","2011-11-01")
#' time5 <- stilf_interval("2011-08-01","2011-09-01")
#' time6 <- stilf_interval("2011-08-15","2011-09-15")
#' time7 <- stilf_interval("2011-08-01","2011-09-15")
#' time8 <- stilf_interval("2011-08-15","2011-08-29")
#' 
#'}
#'
#'

##########################################
# Transform two dates in an interval
##########################################

# Transform two dates in an interval
stilf_interval <- function (first_date, second_date) {
  
  if (!is.null(first_date) & !is.null(second_date) ) {
    
    # test if a valid date
    pattern = c('[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]')
    
    if (grepl(pattern, first_date) == TRUE & grepl(pattern, second_date) == TRUE ){
       
      lubridate::interval(lubridate::ymd(first_date), lubridate::ymd(second_date))
      
    } else {
      stop("\nEnter with a date in format 'year-month-day' = '2010-01-02'\n")
    }
    
  } else {
    stop("\nDates must be defined! \n")
  }
  
}


#' @title Allens Intervals
#' @name allen_relations
#' @aliases allen_relations
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a set of Allen's interval relations to classified time series data. And return a logical value if an interval is TRUE or FALSE
#' 
#' @usage stilf_relation_before(first_interval , second_interval)
#' @usage stilf_relation_after(first_interval , second_interval)
#' @usage stilf_relation_meets(first_interval , second_interval)
#' @usage stilf_relation_met_by(first_interval , second_interval)
#' @usage stilf_relation_overlaps(first_interval , second_interval)
#' @usage stilf_relation_overlapped_by(first_interval , second_interval)
#' @usage stilf_relation_starts(first_interval , second_interval)
#' @usage stilf_relation_started_by(first_interval , second_interval)
#' @usage stilf_relation_during(first_interval , second_interval)
#' @usage stilf_relation_contains(first_interval , second_interval)
#' @usage stilf_relation_finishes(first_interval , second_interval)
#' @usage stilf_relation_finished_by(first_interval , second_interval)
#' @usage stilf_relation_equals(first_interval , second_interval)
#' @usage stilf_relation_in(first_interval , second_interval)
#' @usage stilf_relation_following(first_interval , second_interval)
#' @usage stilf_relation_preceding(first_interval , second_interval)
#'  
#' @param first_interval  stilf_interval. An interval between two dates.
#' @param second_interval stilf_interval. An interval between two dates.
#' 
#' @keywords datasets
#' @return Logical value if interval are TRUE or FALSE
#' @import lubridate
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
#' time2 <- stilf_interval("2011-09-15","2011-11-01")
#' time3 <- stilf_interval("2011-10-01","2011-11-01")
#' time4 <- stilf_interval("2011-10-01","2011-11-01")
#' time5 <- stilf_interval("2011-08-01","2011-09-01")
#' time6 <- stilf_interval("2011-08-15","2011-09-15")
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
#' # Apply a relation 'before' (end_I < start_J)
#' stilf_relation_before(time1,time4)
#' 
#' # Apply a relation 'after' (start_I > end_J)
#' stilf_relation_after(time4,time1)
#' 
#' # Apply a relation 'meets' (end_I == start_J)
#' stilf_relation_meets(time1,time3)
#' 
#' # Apply a relation 'met by' (end_J == start_I)
#' stilf_relation_met_by(time1,time5)
#' 
#' # Apply a relation 'overlaps' (start_I < start_J) & (end_I > start_J) & (end_I < end_J)
#' stilf_relation_overlaps(time1,time2)
#' 
#' # Apply a relation 'overlapped by' (end_I > start_J) & (start_I < end_J) & (end_I > end_J)
#' stilf_relation_overlapped_by(time1,time6)
#' 
#' # Apply a relation 'starts' (start_I == start_J) & (end_I < end_J)
#' stilf_relation_starts(time5,time7)
#' 
#' # Apply a relation 'started by' (start_I == start_J) & (end_I > end_J)
#' stilf_relation_started_by(time7,time5)
#' 
#' # Apply a relation 'during' (start_I > start_J) & (end_I < end_J))
#' stilf_relation_during(time8,time7)
#' 
#' # Apply a relation 'contains' (start_I < start_J) & (end_I > end_J)
#' stilf_relation_contains(time7,time8)
#' 
#' # Apply a relation 'finishes' (start_I > start_J) & (end_I == end_J)
#' stilf_relation_finishes(time4,time3)
#' 
#' # Apply a relation 'finished by' (start_I < start_J) & (end_I == end_J)
#' stilf_relation_finished_by(time3,time4)
#' 
#' # Apply a relation 'equals' (start_I == start_J) & (end_I == end_J)
#' stilf_relation_equals(time3,time4)
#' 
#' # Apply a relation 'in'  (during(first_interval, second_interval) | 
#' # starts(first_interval, second_interval) | 
#' # finishes(first_interval, second_interval))
#' stilf_relation_in(time8,time7)
#' 
#' # Apply a relation 'following' (meets(first_interval, second_interval) |
#' # before(first_interval, second_interval))
#' stilf_relation_following(time1,time3)
#' 
#' # Apply a relation 'preceding' (met_by(first_interval, second_interval) |
#' # after(first_interval, second_interval))
#' stilf_relation_preceding(time1,time3)
#'          
#'            
#'}
#'
#'

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
# following     (meets(first_interval, second_interval) | before(first_interval, second_interval))
# preceding     (met_by(first_interval, second_interval) | after(first_interval, second_interval))


##########################################
# Allen's Temporal Intervals Relations
##########################################

# 1. The '<' relation = stilf_relation_before
stilf_relation_before <- function (first_interval, second_interval) {
  # checking if first or second interval values are correct 
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_end(first_interval) < lubridate::int_start(second_interval)
}


# 2. The '>' relation = stilf_relation_before
stilf_relation_after <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_end(first_interval) > lubridate::int_start(second_interval)
}


# 3. The 'm' relation = stilf_relation_meets
stilf_relation_meets <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_end(first_interval) == lubridate::int_start(second_interval)
}


# 4. The 'mi' relation = stilf_relation_met_by
stilf_relation_met_by <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_end(second_interval) == lubridate::int_start(first_interval)
}


# 5. The 'o' relation = stilf_relation_overlaps
stilf_relation_overlaps <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) < lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) > lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) < lubridate::int_end(second_interval)
}

# 6. The 'oi' relation = stilf_relation_overlapped_by
stilf_relation_overlapped_by <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_end(first_interval) > lubridate::int_start(second_interval) & 
    lubridate::int_start(first_interval) < lubridate::int_end(second_interval) & 
    lubridate::int_end(first_interval) > lubridate::int_end(second_interval)
}

# 7. The 's' relation = stilf_relation_starts
stilf_relation_starts <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) == lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) < lubridate::int_end(second_interval)
}

# 8. The 'si' relation = stilf_relation_started_by
stilf_relation_started_by <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) == lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) > lubridate::int_end(second_interval)
}

# 9. The 'd' relation = stilf_relation_during
stilf_relation_during <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) > lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) < lubridate::int_end(second_interval)
}

# 10. The 'di' relation = stilf_relation_contains
stilf_relation_contains <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) < lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) > lubridate::int_end(second_interval)
}

# 11. The 'f' relation = stilf_relation_finishes
stilf_relation_finishes <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) > lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) == lubridate::int_end(second_interval)
}

# 12. The 'fi' relation = stilf_relation_finished_by
stilf_relation_finished_by <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) < lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) == lubridate::int_end(second_interval)
}


# 13. The 'e' relation = stilf_relation_equals
stilf_relation_equals <- function (first_interval, second_interval) {
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  lubridate::int_start(first_interval) == lubridate::int_start(second_interval) & 
    lubridate::int_end(first_interval) == lubridate::int_end(second_interval)
}


##########################################
# EXTRA RELATIONS
##########################################

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


# 15. The 'stilf_relation_following' relation = stilf_relation_meets v stilf_relation_before
stilf_relation_following <- function(first_interval, second_interval){
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  stilf_relation_before(first_interval, second_interval) | 
    stilf_relation_meets(first_interval, second_interval)
}


# Antonyms of following
# 16. The 'stilf_relation_preceding' relation = stilf_relation_met_by || stilf_relation_after
stilf_relation_preceding <- function(first_interval, second_interval){
  stopifnot(c(lubridate::is.interval(first_interval), 
              lubridate::is.interval(second_interval)))
  
  first_interval <- lubridate::int_standardize(first_interval)
  
  second_interval <- lubridate::int_standardize(second_interval)
  
  stilf_relation_after(first_interval, second_interval) | 
    stilf_relation_met_by(first_interval, second_interval)
}


