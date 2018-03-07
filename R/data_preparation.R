#' Get significant earthquake data
#' @description Retrieve the significant earthquake database from the NOAA website.
#'
#' The Significant Earthquake Database contains information on destructive earthquakes from 2150 B.C. to the present
#' that meet at least one of the following criteria:
#' \itemize{
#' \item moderate damage, approximately $1 million or more
#' \item 10 or more deaths
#' \item magnitude 7.5 or greater
#' \item modified Mercalli Intensity X or greater
#' \item the earthquake generated a tsunami.
#' }
#'
#' The database should be reference to as: National Geophysical Data Center / World Data Service (NGDC/WDS): Significant Earthquake Database. National Geophysical Data Center, NOAA. doi:10.7289/V5TD9V7K
#'
#' More information can be found at the \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{NOAA website}
#'
#' @return Returns a dataframe with earthquake data
#'
#' @importFrom readr read_delim
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_eq_data()
#' }
get_eq_data <- function(){

  url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
  readr::read_delim(url,delim="\t")
}


#' Clean NOAA earthquake data
#'
#' @description This function adds a date column to the NOAA significant earthquake date and converts longtitude and latitude to numeric
#'
#' @param data a dataframe with raw data from the significant earthquake database. This data can be retrieved by the function \code{get_eq_data()}
#'
#' @return This function returns the NOAA earthquake data with an added date column and coordinates converted to numeric class.
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate make_date
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' clean_data <- eq_clean_data(raw_data)
eq_clean_data <- function(data){
  data <- data %>% dplyr::mutate(date = lubridate::make_date(year = YEAR, month = ifelse(is.na(MONTH),1,MONTH), day = ifelse(is.na(DAY),1,DAY)),
                                 LATITUDE = as.numeric(LATITUDE),
                                 LONGITUDE = as.numeric(LONGITUDE),
                                 EQ_PRIMARY = as.numeric(EQ_PRIMARY)
                                 )
}


#' Clean location name of NOAA earthquake data
#'
#' @description
#' The NOAA significant earthquake data has some excess information in the column \code{LOCATION_NAME} concerning the country of the earthquake.
#' This function removes the country from the location name leaving only the location itself.
#'
#' @param data a dataframe with raw data from the significant earthquake database
#'
#' @return returns a dataframe where the contents of the column \code{LOCATION_NAME} have been stripped of the country name
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace
#' @importFrom tools toTitleCase
#'
#' @note In some rare cases the \code{LOCATION_NAME} column has more than one country and location.
#' This function will keep only the last location that is present in the column. It removes all information before the last colon (:).
#'
eq_location_clean <- function(data){
  data <- data %>% dplyr::mutate(LOCATION_NAME = trimws(stringr::str_replace(LOCATION_NAME, pattern = "^.*:[:space:]{1}", replacement = ""))) %>%
    mutate(LOCATION_NAME = tools::toTitleCase(tolower(LOCATION_NAME))) # calling mutate on LOCATION_NAME twice to avoid excessive nesting
  }

