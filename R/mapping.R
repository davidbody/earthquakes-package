#' Map earthquakes.
#'
#' Produces a map of earthquake epicenters (LATITUDE/LONGITUDE) and annotates
#' each point with a popup window containing annotation data stored in a column
#' of the data frame. The radius of each circle is proportional to the
#' earthquake's magnitude (EQ_PRIMARY).
#'
#' @param df Data frame containing at least LONGITUDE, LATITUDE, and EQ_PRIMARY
#'     columns.
#' @param annot_col Character string with the name of the column to use for the
#'     popup windows.
#'
#' @examples
#' \dontrun{
#' eq_map(earthquakes, annot_col = "DATE")
#'}
#' @note
#' See \code{\link{eq_create_label}} for another example.
#'
#' @export
eq_map <- function(df, annot_col) {
    leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = df, lng = ~LONGITUDE, lat = ~LATITUDE,
                                  radius = ~EQ_PRIMARY, weight = 1,
                                  popup = df[[annot_col]])
}

#' Create HTML labels for earthquakes
#'
#' Creates HTML labels with location, magnitude, and total deaths for
#' earthquake data.
#'
#' @param df Data frame containing at least LOCATION_NAME, EQ_PRIMARY, and
#'     TOTAL_DEATHS columns.
#' @return Character string with the HTML label.
#'
#' @examples
#' \dontrun{
#' # This can be used with \code{\link{eq_map}} as follows:
#' library(dplyr)
#'
#' earthquakes %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'}
#' @export
eq_create_label <- function(df) {
    paste(sep = "<br/>",
          paste("<b>Location:</b>", df$LOCATION_NAME),
          paste("<b>Magnitude:</b>", df$EQ_PRIMARY),
          paste("<b>Total deaths:</b>", df$TOTAL_DEATHS)
    )
}
