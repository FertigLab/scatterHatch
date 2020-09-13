# Preparing helper functions to create the legend for graph
# Tejas Guha - September 13, 2020


#' Creating a Legend Icon
#'
#' This function creates the legend icons for the four distinct patterns supported at the moment:
#' horizontal, vertical, positiveDiagnol, and negativeDiagnol.  The legend icons are rendered using
#' grob raster objects in the grid package.  Before the grid method, the icons were drawn using
#' image_draw in the Magick package.  The Magick method was proven impractical because it created
#' an issue with the graphic device preventing ggsave from rendering the final plot.
#'
#' @param color Background color of the icon
#' @param lineColor Color of the pattern lines
#' @param lineType Line type of pattern lines (e.g. dashed, dotted, etc.)
#' @param patternType Pattern Type of icon (e.g. vertical, horizontal, etc.)
#' @return List of grob objects
#' @export
legendIcon <- function(color, lineColor="black", lineType = "solid", patternType, lineWidth=1.5, lineAlpha=0.4){
  linePar = grid::gpar(col = lineColor, fill = color, lwd = 2*lineWidth, lty = lineType, alpha=1)
  if (patternType == "horizontal"){
    return(grid::grobTree(
      grid::circleGrob(0.5, 0.5, 0.5, gp=grid::gpar(col=color, fill=color, lwd=1, alpha=1)),
      grid::linesGrob(x=c(0,1), y=c(0.5, 0.5), gp=linePar),
      grid::linesGrob(x=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), y=c(0.25, 0.25), gp=linePar),
      grid::linesGrob(x=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), y=c(0.75, 0.75), gp=linePar),
      gp=grid::gpar(alpha=1)))
  }
  if (patternType == "vertical"){
    return(grid::grobTree(
      grid::circleGrob(0.5, 0.5, 0.5, gp=grid::gpar(col=color, fill=color, lwd=1, alpha=1)),
      grid::linesGrob(y=c(0,1), x=c(0.5, 0.5), gp=linePar),
      grid::linesGrob(y=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), x=c(0.25, 0.25), gp=linePar),
      grid::linesGrob(y=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), x=c(0.75, 0.75), gp=linePar),
      gp=grid::gpar(alpha=1)))
  }
  if (patternType == "positiveDiagonal"){
    return(grid::grobTree(
      grid::circleGrob(0.5, 0.5, 0.5, gp=grid::gpar(col=color, lwd=1, alpha=1, fill=color)),
      grid::linesGrob(x=c(-sqrt((0.5)^2/2)+0.5,sqrt((0.5)^2/2)+0.5), y=c(-sqrt((0.5)^2/2)+0.5, sqrt((0.5)^2/2)+0.5), gp=linePar),
      grid::linesGrob(x=c(0.01779486, 0.6322051), y=c(0.3677949, 0.9822051), gp=linePar),
      grid::linesGrob(x=c(0.3677949, 0.9822051), y=c(0.01779486, 0.6322051), gp=linePar),
      gp=grid::gpar(alpha=1)
      ))
  }
  if (patternType == "negativeDiagonal"){
    return(grid::grobTree(
      grid::circleGrob(0.5, 0.5, 0.5, gp=grid::gpar(col=color, fill=color, lwd=1, alpha=1)),
      grid::linesGrob(x=c(-sqrt((0.5)^2/2)+0.5,sqrt((0.5)^2/2)+0.5), y=c(sqrt((0.5)^2/2)+0.5, -sqrt((0.5)^2/2)+0.5), gp=linePar),
      grid::linesGrob(x=c(0.01779486, 0.6322051), y=c(0.6322051, 0.01779486), gp=linePar),
      grid::linesGrob(x=c(0.3677949, 0.9822051), y=c(0.9822051, 0.3677949), gp=linePar),
      gp=grid::gpar(alpha=1)))
  }
}

#' Creates custom ggplot2 object
#'
#' ggplot2 object that transfers the pattern aesthetics (e.g. color, lineColor, etc.) to render the legend icon.
#'
imagePoints <- ggplot2::ggproto("imagePoints", ggplot2::Geom,
                       required_aes = c("x", "y", "ids"),
                       default_aes = ggplot2::aes(size = 5, shape=19),

                       draw_key = function (data, params, size)
                       {
                         iconInfo = data$ids[[1]] # ids contains all the necessary info to render icon
                         legendIcon(iconInfo[[1]], iconInfo[[2]], iconInfo[[3]], iconInfo[[4]], iconInfo[[5]], iconInfo[[6]])
                       },

                       draw_group = function(data, panel_scales, coord) {
                         coords <- coord$transform(data, panel_scales)
                         grid::pointsGrob(coords$x, coords$y, unit(0, "char"), pch = coords$shape,
                                          gp = grid::gpar(col = "black"))
                       }
)


geom_imagePoint <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = imagePoints, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
