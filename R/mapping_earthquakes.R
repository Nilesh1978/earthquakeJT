#' Create Leaflet Earthquake map
#'
#' This function creates a Leaflet map with the locations of earthquakes from the NOAA Earthquake database
#'
#' @param data A (filtered and) cleaned dataframe with locations from the NOAA Earthquake database
#' @param annot_col A character string with the column that should be used for the pop-ups on the map
#'
#' @import leaflet
#' @importFrom dplyr %>%
#'
#' @return A Leaflet map widget with the plotted locations of Earthquakes
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_eq_data() %>%
#'  eq_clean_data() %>%
#'  eq_map(annot_col = "date")
#'}
eq_map <- function(data, annot_col=NA){

  leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius = ~EQ_PRIMARY, weight = 2, popup = data[[annot_col]] ) #%>%
    #addPopups(lng = ~LONGITUDE, lat = ~LATITUDE, popup = annot_col, options = popupOptions())
}


#' Create a Label for the Earthquake map
#'
#' This function adds a column \code{popup_text}  to the earthquake dataframe. This column can be used for the annotation column in \code{eq_map()}
#'
#' @param data a (cleaned) dataframe with earthquake data from the NOAA earthquake database
#'
#' @return The input dataframe with an additional column called popup_text, which contains (when available) in html the Location,
#' the Magnitude and the Total number of deaths for each earthquake
#'
#' @importFrom dplyr mutate %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' eq_create_label(data = get_eq_data() )
#' }
eq_create_label <- function(data){
  LOCATION_NAME <- TOTAL_DEATHS <- EQ_PRIMARY <- NULL # to avoid NOTES when building the package
  data %>% dplyr::mutate(popup_text = paste0(ifelse(is.na(LOCATION_NAME), "", paste0("<b>Location: </b>", LOCATION_NAME)),
                                    ifelse(is.na(EQ_PRIMARY), "", paste0("</br><b>Magnitude: </b>",EQ_PRIMARY)),
                                    ifelse(is.na(TOTAL_DEATHS), "", paste0("</br><b>Total deaths: </b>", TOTAL_DEATHS))
  ))
}


