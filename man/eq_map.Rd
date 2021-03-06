% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_map.R
\name{eq_map}
\alias{eq_map}
\title{Creating an interactive map of the epicenters from NOAA eqrthquake data}
\usage{
eq_map(df_map, annot_col = "DATE")
}
\arguments{
\item{df_map}{data frame. a cleaned data frame containing NOAA eqrthquake information}

\item{annot_col}{character. to specify the column which should be shown as an annotation in the leaflet map}
}
\value{
Returns an interactive HTML-map of the earthquake epicenters.
}
\description{
Creating an interactive map of the epicenters from NOAA eqrthquake data
}
\details{
This function returns an interactive map of the NOAA eqrthquake data. The epicenters constructed
with the longitude- and lattitude information from the cleaned NOAA data frame. It is possible to change the column,
which should represent the annotation in the pop-up text in the map (default is set to \code{DATE}). The size of the
circle represents the magnitude of the earthquake and is specified by the column: \code{EQ_PRIMARY} from the NOAA data set.
}
\examples{
# the data first has to be cleaned by the function eq_clean_data
\dontrun{
library(magrittr)
readr::read_delim("signif.txt", delim = "\\t") \%>\%
eq_clean_data() \%>\%
  dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 1990) \%>\%
  eq_map(annot_col = "DATE")
}

}
