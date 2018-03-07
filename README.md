# earthquakeJT

The goal of earthquakeJT is to retrieve and visualize earthquake data from the NOAA significant earthquake database. 

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)

data <- get_eq_data() %>%     # download data
      eq_clean_data() %>%     # clean the downloaded data
      eq_location_clean()     # clean the description of the earthquake location


ggplot() + 
  geom_timeline(data = data, aes(x = date) +              # create a timeline
  geom_timeline_label(data = data, label = LOCATION_NAME) # add labels to the timeline
  
data %>% 
  eq_create_label() %>%             # adds to data a column "popup_text"", based on location, magnitude and number of casualties
  eq_map(annot_col = "popup_text")  # create a leaflet map with earthquake locations, with pop_ups


```
