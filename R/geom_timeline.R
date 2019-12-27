#' Plotting a timeline of earthquakes
#'
#' @inheritParams ggplot2::layer
#' @param mapping aesthetic mappings
#' @param data data to be visualized by the geom
#' @param stat character. The statistical transformation to use on the data for this layer, as a string
#' @param position character. Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param na.rm logical. variable to indicate if NAs should be removed from the data
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes logical. If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification.
#' @param ... Other arguments passed on to the layer
#'
#' @section Aestethics
#' \itemize{
#' \item \code{x}          #Time variable
#' \item \code{y}          #Factor. variable corresponding to \code{x}
#' \item \code{color}      #Color of objects
#' \item \code{shape}      #Shape
#' \item \code{size}       #Size
#' \item \code{alpha}      #Transparency
#' \item \code{fill}       #interiour couloring of objects
#' \item \code{stroke}     #Stroke
#' }
#'
#'
#' @return Returns a layer, a combination of data, stat and geom objects.
#'
#' @examples
#' #Before running the function on the data,
#' #one needs to read in the NOAA data and clean the data frame in order
#' #for the function to work properly.
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_clean_data(data)
#'  data %>%
#'  dplyr::filter(COUNTRY == c("MEXICO","China") & lubridate::year(DATE) >= 2010) %>%
#'  ggplot(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY, label=LOCATION_NAME)) +
#'  geom_timeline(alpha=0.7) +
#'  theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#'  ggtitle("Earthquakes Timeline") +
#'  labs(size = "Richter scale value", color = "# Deaths")
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom: Timeline for NOAA data
#'
#' GeomTimeline represents the source code for the geom \code{TimeLine} without labels.
#' It corresponds with \code{geom_timeline} and represents the geom object that is returned as part of the layer.
#'
#' @importFrom ggplot2 ggproto draw_key_point Geom aes
#' @importFrom grid gpar gList pointsGrob segmentsGrob
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y=0, colour="black", shape=19, size=1, stroke = 0.5, alpha = 0.5, fill = NA),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   grid::gList(
                                     grid::pointsGrob(
                                       coords$x, coords$y,
                                       pch = coords$shape,
                                       gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                       fill = alpha(coords$fill, coords$alpha),
                                                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                       lwd = coords$stroke * .stroke / 2
                                       )
                                     ),
                                     grid::segmentsGrob(
                                       x0 = min(coords$x), y0 = coords$y, x1 = max(coords$x), y1 = coords$y,
                                       gp = grid::gpar(col = "gray", lwd = 1)
                                     )
                                   )
                                 }
)





#' Adding labels to a geom_timeline layer
#'
#' @inheritParams ggplot2::layer
#' @param mapping aesthetic mappings
#' @param data data to be visualized by the geom
#' @param stat character. The statistical transformation to use on the data for this layer, as a string
#' @param position character. Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param na.rm logical. variable to indicate if NAs should be removed from the data
#' @param show.legend logical. Should this layer be included in the legends?
#' NA, the default, includes if any aesthetics are mapped.
#'  FALSE never includes, and TRUE always includes.
#'  It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes logical. If FALSE, overrides the default aesthetics, rather than combining with them.
#'  This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification.
#' @param ... Other arguments passed on to the layer
#'
#' @section Aestethics
#' \itemize{
#' \item \code{x}          #Time variable
#' \item \code{y}          #Factor. variable corresponding to \code{x}
#' \item \code{color}      #Color of objects
#' \item \code{shape}      #Shape
#' \item \code{size}       #Size
#' \item \code{alpha}      #Transparency
#' \item \code{fill}       #interiour couloring of objects
#' \item \code{stroke}     #Stroke
#' }
#'
#'
#' @return Returns a layer, a combination of data, stat and geom objects including labels for certain data-points.
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' #
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_clean_data(data)
#' data %>%
#' dplyr::filter(COUNTRY == c("USA","CHINA") & lubridate::year(DATE) >= 2008) %>%
#'  ggplot(aes(x=DATE,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
#'  geom_timeline(alpha=.5) +
#'  geom_timelinelabel(aes(label=LOCATION_NAME),n_max=3) +
#'  theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#'  ggtitle("Earthquakes Visualization Tool") +
#'  labs(size = "Richter scale value", color = "# Deaths")
#'
#' }
#'
#' @export
geom_timelinelabel <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelinelabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}




#' Geom: Adding labels to the Timeline-Geom
#'
#' GeomTimeline represents the source code for the geom \code{TimeLine} with labels.
#' It corresponds with \code{geom_timeline} and adds label objects to the timeline layer.
#'
#' @importFrom ggplot2 ggproto draw_key_point Geom aes
#' @importFrom grid gpar gList pointsGrob segmentsGrob
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
GeomTimelinelabel <- ggplot2::ggproto("GeomTimelinelabel", ggplot2::Geom,
                                      required_aes = c("x","label"),
                                      default_aes = ggplot2::aes(y=0, n_max=0, y_length=1),
                                      draw_key = ggplot2::draw_key_point,
                                      draw_panel = function(data, panel_params, coord) {

                                        data<- data %>%
                                          dplyr::group_by(data$y) %>%
                                          dplyr::arrange_(~ desc(size)) %>%
                                          dplyr::slice(1:data$n_max[1])


                                        coords <- coord$transform(data, panel_params)

                                        grid::gList(
                                          grid::segmentsGrob(
                                            x0 = coords$x, y0 = coords$y, x1 = coords$x, y1 = 0.1+coords$y,
                                            gp = grid::gpar(col = "black", lwd = .5)
                                          ),
                                          grid::textGrob(
                                            label = coords$label,
                                            x = coords$x, y = 0.11+coords$y , just = "left", rot = 45,
                                            gp =grid:::gpar(fontsize=8)
                                          )
                                        )
                                      }
)
