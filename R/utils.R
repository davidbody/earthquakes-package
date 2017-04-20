#' Make a Date from year, month, day
#'
#' This function makes a Date from the supplied year, month, and day paramters.
#' NA values for month and day are replaced by 1.
#'
#' Currently requires year to be >= 0.
#'
#' @param year Numeric year
#' @param month Numeric month
#' @param day Numeric day
#' @return A Date object
#'
#' @examples
#' earthquakes:::to_date(2017, 4, 17)  # => 2017-04-17
#' earthquakes:::to_date(2017, 4, NA)  # => 2017-04-01
#' earthquakes:::to_date(2017, NA, NA) # => 2017-01-01
#'
#' @note This function is for internal use only.
to_date <- function(year, month, day) {
    if (is.na(month)) {
        month <- 1
    }

    if (is.na(day)) {
        day <- 1
    }

    if (year >= 0) {
        as.Date(paste(year, month, day, sep = "-"))
    } else {
        NA
    }
}
