library(ggplot2)
#library(devtools)
#library(dplyr)
#library(readr)
#library(stringr)
#library(lubridate)
library(grid)

#Build a geom for ggplot2 called geom_timeline() for plotting a time line of earthquakes ranging from xmin to xmax dates
#with a point for each earthquake. Optional aesthetics include color, size, and alpha (for transparency).
#The x aesthetic is a date and an optional y aesthetic is a factor indicating some stratification
#in which case multiple time lines will be plotted for each level of the factor (e.g. country).


GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                           required_aes = c("x"),
                           default_aes = aes(shape = 19,
                                             colour = "black",
                                             size = 1.5,
                                             alpha = NA,
                                             stroke = 0.5,
                                             fill="black",
                                             y = 0.25),

                           draw_key = draw_key_point,

                           draw_panel = function(data, panel_scales, coord) {
                             coords <- coord$transform(data, panel_scales)

                             # creates a line on which the earthquakes are plotted
                             y_values <- unique(coords$y)
                             no_y <- length(y_values)
                             eq_line <- grid::polylineGrob(x = rep(c(0,1),no_y), y = sort(rep(y_values,2)), id=sort(rep(c(1:no_y),2)), gp=grid::gpar(col = "grey", lwd = 3))
                             #creates the points on the timeline
                             eq_points <- grid::pointsGrob(
                               coords$x, coords$y,
                               pch = coords$shape,
                               size = unit(coords$size*.pt, "mm"),
                               gp = gpar(col = scales::alpha(coords$colour, coords$alpha),
                                         fill = scales::alpha(coords$fill, coords$alpha),
                                         fontsize = coords$size * .pt + coords$stroke * .stroke/2,
                                         lwd = coords$stroke * .stroke/2)
                               )
                             plot <- grid::gTree(children = grid::gList(eq_line, eq_points))

                           }
)


geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  theme_light()
  layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#2. Build a geom called geom_timeline_label() for adding annotations to the earthquake data.
#This geom adds a vertical line to each data point with a text annotation (e.g. the location of the earthquake) attached to each line.
#There should be an option to subset to n_max number of earthquakes, where we take the n_max largest (by magnitude) earthquakes.
#Aesthetics are x, which is the date of the earthquake and label which takes the column name from which annotations will be obtained.

GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = aes(shape = 19, colour = "black", size = 1.5, alpha = NA, stroke = 0.5, fill="black", y = 0.25),

                                      draw_key = draw_key_point,

                                      draw_panel = function(data, panel_scales, coord, n_max) {

                                        if(!is.na(n_max)){data <- data %>% dplyr::arrange(-magnitude) %>% head(n_max)}
                                        coords <- coord$transform(data, panel_scales)
                                        str(coords)
                                        # creates the label lines
                                        #y_values <- unique(coords$y)
                                        no_y <- length(unique(coords$y))
                                        label_line <- grid::polylineGrob(x = c(rbind(coords$x,coords$x)),
                                                                         y = c(rbind(coords$y,coords$y + (1/(no_y+4)))),
                                                                         id=sort(rep(c(1:length(coords$x)),2)),
                                                                         gp=grid::gpar(col = "grey", lwd = 2)
                                                                         )
                                        #creates the labels
                                        label_text <- grid::textGrob(label = coords$label,
                                                                     x = coords$x,
                                                                     y = coords$y + (1/(no_y+3.8)),
                                                                     rot = 45,
                                                                     just = "left"

                                        )
                                        plot <- grid::gTree(children = grid::gList(label_line, label_text))

                                      }
)


# when using n_max, the aes magnitude needs to be provided
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, n_max = NA, ...){

  theme_light()
  layer(
    geom = GeomTimelineLabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n_max = n_max, na.rm = na.rm, ...)
  )
}


#TESTREGEL
#ggplot() + geom_timeline(data=data,aes(x=date,colour=YEAR)) + theme_minimal()
#summary(as.factor(data$COUNTRY))
#test <- dplyr::filter(data, COUNTRY %in% c("CHINA","JAPAN","USA"),date > as.POSIXlt.date("2016-01-01"))

#ggplot(data=test,aes(x=date, y=COUNTRY, colour=EQ_PRIMARY ,size= EQ_PRIMARY, alpha=0.1 )) + geom_timeline() + theme_minimal()
#ggplot(data=test,aes(x=date, y=COUNTRY, colour=EQ_PRIMARY ,size= EQ_PRIMARY, alpha=0.1, label = LOCATION_NAME )) + geom_timeline() + geom_timeline_label() + theme_minimal()
#ggplot(data=test,aes(x=date, colour=EQ_PRIMARY ,size= EQ_PRIMARY, alpha=0.1, label = LOCATION_NAME)) +  geom_timeline_label() + geom_timeline() + theme_minimal()


#ggplot() + geom_timeline(data=test,aes(x=date, y=COUNTRY, colour=EQ_PRIMARY ,size= EQ_PRIMARY, alpha=0.1 )) + theme_minimal()
#ggplot() + geom_timeline(data=test,aes(x=date, colour=EQ_PRIMARY ,size= EQ_PRIMARY, alpha=0.1 )) + theme_minimal()


