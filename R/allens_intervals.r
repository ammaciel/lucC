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
##                                             2016-08-22      ##
##                                                             ##
##  J. F. Allen.  Towards a general theory of action and       ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


#' @title Allens Intervals
#' @name allen_intervals
#' @aliases allen_intervals
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a set of Allen's intreval operations to classified time series data. And return a logical value if a interval is TRUE or FALSE
#' @usage after(fileTS = NULL, nameColumnValue = NULL,
#' subInterval = FALSE, numberSubIntervals = 2)
#' @param fileTS Dataframe. A time series file.
#' @param nameColumnValue Character. Name of a column with value, such as, Vegetation Index EVI or NDVI.
#' @param subInterval Logical. If TRUE, the data.frame will be divide in subintervals. Given a data.frame with 23 rows and 3 subintervals, the first and second subinterval will have 8 rows each, and the last only 7 rows. If FALSE (the default) nothing subinterval will be created.
#' @param numberSubIntervals Integer. Number of subintervals to feature extraction.
#' @keywords datasets
#' @return Dataframe with statistical features, fbgf
#' @import lubridate
#' @export
#'
#' @examples \dontrun{
#' # Open a data example
#' library(stilf)
#' 
#' # Apply the filterTS function on df data frame
#' dataFiltered <- filterTS(fileTS = df, nameColumnValue = "value", outlier = TRUE, value= -0.300)
#'
#' # Apply splitTS fcuntion to divide time series for year
#' splitTS(dataFiltered,2002,2005,"date",typeInterval = "annual")
#'
#' # Get features from time series divided in annual values without subintervals
#' example1 <- featuresExtractionTS(fileTS = ts.annual_2002, nameColumnValue =
#' "filtered.value", subInterval = FALSE)
#'
#' # Get features from time series divided in annual values with subintervals
#' example2 <- featuresExtractionTS(fileTS = ts.annual_2002, nameColumnValue =
#' "filtered.value", subInterval = TRUE, numberSubIntervals = 3)
#'
#' # Show data frames example1 and example2 utils::View(example1) utils::View(example2)
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
# in            (during(first_interval, second_interval) | starts(first_interval, second_interval) | finishes(first_interval, second_interval))
# next          (meets(first_interval, second_interval) | before(first_interval, second_interval))


# some examples examples intervals
# library(lubridate)
# 
# time1 <- interval(ymd("2011-09-01"),ymd("2011-10-01"))
# time2 <- interval(ymd("2011-09-15"),ymd("2011-11-01"))
# time3 <- interval(ymd("2011-10-01"),ymd("2011-11-01"))
# time4 <- interval(ymd("2011-10-15"),ymd("2011-11-01"))
# time5 <- interval(ymd("2011-08-01"),ymd("2011-09-01"))
# time6 <- interval(ymd("2011-08-15"),ymd("2011-09-15"))
# time7 <- interval(ymd("2011-08-01"),ymd("2011-09-15"))
# time8 <- interval(ymd("2011-08-15"),ymd("2011-08-29"))

# Transform two dates in an interval
stilf_interval <- function (first_date, second_date) {
 # first_date <- int_standardize(first_date)
#  second_date <- int_standardize(second_date)
  interval(ymd(first_date), ymd(second_date))
}

# Allen's Temporal Intervals Relations

# 1. The '<' relation = stilf_before
stilf_before <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_end(first_interval) < int_start(second_interval)
}
#stilf_before(time1,time4)

# 2. The '>' relation = stilf_before
stilf_after <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_end(first_interval) > int_start(second_interval)
}
#stilf_after(time4,time1)

# 3. The 'm' relation = stilf_meets
stilf_meets <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_end(first_interval) == int_start(second_interval)
}
#stilf_meets(time1,time3)

# 4. The 'mi' relation = stilf_met_by
stilf_met_by <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_end(second_interval) == int_start(first_interval)
}
#stilf_met_by(time1,time5)

# 5. The 'o' relation = stilf_overlaps
stilf_overlaps <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) < int_start(second_interval) & int_end(first_interval) > int_start(second_interval) & int_end(first_interval) < int_end(second_interval)
}
#stilf_overlaps(time1,time2)

# 6. The 'oi' relation = stilf_overlapped_by
stilf_overlapped_by <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_end(first_interval) > int_start(second_interval) & int_start(first_interval) < int_end(second_interval) & int_end(first_interval) > int_end(second_interval)
}
#stilf_overlapped_by(time1,time6)

# 7. The 's' relation = stilf_starts
stilf_starts <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) == int_start(second_interval) & int_end(first_interval) < int_end(second_interval)
}
#stilf_starts(time5,time7)

# 8. The 'si' relation = stilf_started_by
stilf_started_by <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) == int_start(second_interval) & int_end(first_interval) > int_end(second_interval)
}
#stilf_started_by(time7,time5)

# 9. The 'd' relation = stilf_during
stilf_during <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) > int_start(second_interval) & int_end(first_interval) < int_end(second_interval)
}
#stilf_during(time8,time7)

# 10. The 'di' relation = stilf_contains
stilf_contains <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) < int_start(second_interval) & int_end(first_interval) > int_end(second_interval)
}
#stilf_contains(time7,time8)

# 11. The 'f' relation = stilf_finishes
stilf_finishes <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) > int_start(second_interval) & int_end(first_interval) == int_end(second_interval)
}
#stilf_finishes(time4,time3)

# 12. The 'fi' relation = stilf_finished_by
stilf_finished_by <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) < int_start(second_interval) & int_end(first_interval) == int_end(second_interval)
}
#stilf_finished_by(time3,time4)

# 13. The 'e' relation = stilf_equals
stilf_equals <- function (first_interval, second_interval) {
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  int_start(first_interval) == int_start(second_interval) & int_end(first_interval) == int_end(second_interval)
}
#stilf_equals(time3,time4)


# EXTRA
# 14. The 'stilf_in' relation = stilf_during v stilf_starts v stilf_finishes
stilf_in <- function(first_interval, second_interval){
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  stilf_during(first_interval, second_interval) | stilf_starts(first_interval, second_interval) | stilf_finishes(first_interval, second_interval)
}
#stilf_in(time8,time7)

# 15. The 'stilf_next' relation = stilf_meets v stilf_before
stilf_next <- function(first_interval, second_interval){
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  stilf_before(first_interval, second_interval) | stilf_meets(first_interval, second_interval)
}
#stilf_next(time1,time3)

# 16. The 'stilf_preceding' relation = stilf_met_by || stilf_after
stilf_preceding <- function(first_interval, second_interval){
  stopifnot(c(is.interval(first_interval), is.interval(second_interval)))
  first_interval <- int_standardize(first_interval)
  second_interval <- int_standardize(second_interval)
  stilf_after(first_interval, second_interval) | stilf_met_by(first_interval, second_interval)
}
#stilf_preceding(time1,time3)

