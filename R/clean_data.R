#' Clean earthquake location data
#'
#' @param location Charater string with location, including country followed by a colon.
#'
#' @return Returns a character string in title case with the country and colon removed.
#'
#' @examples
#' eq_location_clean("ITALY: VERONA") # => "Verona"
#'
#' @export
eq_location_clean <- function(location) {
    tools::toTitleCase(trimws(tolower(sub("\\S+: +", "", location))))
}

#' Clean earthquake data frame
#'
#' A DATE column is created based on YEAR, MONTH, DAY.
#' LATITUDE and LONGITUDE are converted to numeric.
#' EQ_PRIMARY and TOTAL_DEATHS are converted to numeric.
#' LOCATION_NAME is cleaned by passing it to the \code{\link{eq_location_clean}} function.
#'
#' @param earthquakes A data frame containing the earthquakes data set.
#'
#' @return A new data frame with cleaned earthquakes data.
#'
#' @examples
#' \dontrun{
#' eq_clean_data(earthquakes)
#'}
#' @export
eq_clean_data <- function(earthquakes) {
    result <- data.frame(earthquakes)
    result$DATE <- as.Date(mapply(to_date, result$YEAR,
                                  result$MONTH, result$DAY),
                           origin = as.Date("1970-01-01"))
    result$LATITUDE <- as.numeric(result$LATITUDE)
    result$LONGITUDE <- as.numeric(result$LONGITUDE)
    result$EQ_PRIMARY <- as.numeric(result$EQ_PRIMARY)
    result$TOTAL_DEATHS <- as.numeric(result$TOTAL_DEATHS)
    result$LOCATION_NAME <- eq_location_clean(result$LOCATION)
    result
}
