#' Creating an interactive map of the epicenters from NOAA eqrthquake data
#'
#' @param df_map data frame. a cleaned data frame containing NOAA eqrthquake information
#' @param annot_col character. to specify the column which should be shown as an annotation in the leaflet map
#'
#' @details This function returns an interactive map of the NOAA eqrthquake data. The epicenters constructed
#' with the longitude- and lattitude information from the cleaned NOAA data frame. It is possible to change the column,
#' which should represent the annotation in the pop-up text in the map (default is set to \code{DATE}). The size of the
#' circle represents the magnitude of the earthquake and is specified by the column: \code{EQ_PRIMARY} from the NOAA data set.
#'
#' @return Returns an interactive HTML-map of the earthquake epicenters.
#'
#'
#' @examples
#' # the data first has to be cleaned by the function eq_clean_data
#' \dontrun{
#' library(magrittr)
#' readr::read_delim("signif.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 1990) %>%
#'   eq_map(annot_col = "DATE")
#' }
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr %>%
#'
#' @export
eq_map <- function(df_map, annot_col = "DATE") {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = df_map$LONGITUDE, lat = df_map$LATITUDE,
                              radius = as.numeric(df_map$EQ_PRIMARY), popup = df_map[[annot_col]],
                              stroke = FALSE, fillOpacity = 0.5)
}

#' Create Pop-Ups to be utilized by the function eq_map()
#'
#' @param df_map data frame. cleaned NOAA data
#'
#' @details Takes the dataset as an argument and creates an HTML label that can be used as the annotation text in the leaflet map.
#'
#' @return Returns a string/HTML label with the right format for the columns \code{LOCATION_NAME,EQ_PRIMARY,TOTAL_DEATHS} as an annotation text in the leaflet map.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' readr::read_delim("signif.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#' @export
eq_create_label <- function(df_map){
  paste(ifelse(is.na(df_map$LOCATION_NAME),"", paste("<b>Location: </b>",df_map$LOCATION_NAME,"<br/>")),
        ifelse(is.na(df_map$EQ_PRIMARY),"", paste("<b>Magnitude: </b>",df_map$EQ_PRIMARY,"<br/>")),
        ifelse(is.na(df_map$TOTAL_DEATHS),"", paste("<b>Total deaths: </b>",df_map$TOTAL_DEATHS,"<br/>")))
}
