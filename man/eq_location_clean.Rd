% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_data.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Clean earthquake location data}
\usage{
eq_location_clean(location)
}
\arguments{
\item{location}{Charater string with location, including country followed by a colon.}
}
\value{
Returns a character string in title case with the country and colon removed.
}
\description{
Clean earthquake location data
}
\examples{
eq_location_clean("ITALY: VERONA") # => "Verona"

}
