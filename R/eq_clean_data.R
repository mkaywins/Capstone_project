utils::globalVariables(c("LOCATION_NAME","I_D","YEAR","MONTH","DAY","LATITUDE","LONGITUDE",
                         "EQ_PRIMARY","COUNTRY","STATE","TOTAL_DEATHS","DATE","YEAR_temp"))

#' Cleaning raw NOAA data frame and returning a clean data frame
#'
#' @param df A raw data frame containing eqrthquake data from NOAA website
#'
#' @return A clean data frame with cleaned date, latitude,longitude and death columns
#'
#' @details In order to use this function, one has to obtain the NOAA data from
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}. The function returns
#' a cleaned data frame with cleaned columns: \code{date,LONGITUDE,LATITUDE,DEATHS}. The \code{date} column is
#' converted into date-format and the \code{LONGITUDE,LATITUDE,DEATHS} are converted into numeric types.
#' The function utilizes the function \code{eq_location_clean}.
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_clean_data(data)
#' }
#'
#'@importFrom dplyr mutate if_else
#'@importFrom magrittr %>%
#'@importFrom tidyr unite
#'@importFrom lubridate ymd year
#'
#' @export
eq_clean_data <- function(df) {
  df <- df %>%
    dplyr::mutate(YEAR_temp=sprintf("%04d",as.numeric(gsub('-','',YEAR)))) %>%
    dplyr::mutate(MONTH=dplyr::if_else(is.na(MONTH),'01',sprintf("%02d", MONTH))) %>%
    dplyr::mutate(DAY=dplyr::if_else(is.na(DAY),'01',sprintf("%02d", DAY))) %>%
    tidyr::unite(DATE,YEAR_temp,MONTH,DAY,sep='-',remove = FALSE) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    dplyr::select(-YEAR_temp)

  lubridate::year(df$DATE) <- df$YEAR

  df <- df %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),LONGITUDE = as.numeric(LONGITUDE),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY), TOTAL_DEATHS = as.numeric(TOTAL_DEATHS))

  df<-eq_location_clean(df)

  return(df)
}



#' Cleaning raw NOAA data frame location columns
#'
#' @param df A raw data frame containing eqrthquake data from NOAA website
#'
#' @return A clean data frame with location column
#'
#' @details In order to use this function, one has to obtain the NOAA data from
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}. The function returns
#' a cleaned data frame with cleaned columns: \code{LOCATION_NAME}. From the \code{LOCATION_NAME}
#' column the country name is stripped out and names are converted to title case.
#'
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_location_clean(data)
#' }
#'
#'@importFrom dplyr mutate
#'@importFrom stringr str_to_title
#'@importFrom magrittr %>%
#'
#' @export
eq_location_clean<-function(df){
  LOCATION_NAME<-NULL
  df<-df %>% dplyr::mutate(LOCATION_NAME=stringr::str_to_title(LOCATION_NAME))
  return(df)
}
