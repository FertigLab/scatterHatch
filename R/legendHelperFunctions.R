## Preparing helper functions to create the legend for graph
## Tejas Guha - September 13, 2020


#' Creating a Legend Icon
#'
#' This function creates the legend icons for the patterns 
#' inputted to scatterHatch. The legend icons are rendered using grob raster 
#' objects in the grid package.
#'
#' @param color Background color of the icon
#' @param lineColor Color of the pattern lines
#' @param lineType Line type of pattern lines (e.g. dashed, dotted, etc.)
#' @param patternType Pattern Type of icon (e.g. vertical, horizontal, etc.)
#' @param lineWidth Width of the line
#' @param angle Vector of the line angles for a given pattern
#' @return List of grob objects
#' @noRd
legendIcon <- function(color, lineColor="black", lineType="solid", 
    patternType, lineWidth=1.5, angle=NULL) {
    radius <- 0.35
    linePar <- grid::gpar(col = lineColor, fill = color, lwd = 2 * lineWidth, 
    lty = lineType, alpha = 1)
    circle <- grid::circleGrob(0.5, 0.5, radius, gp = grid::gpar(col = color, 
    fill = color, lwd = 1, alpha = 1))
    grobs <- list(circle)
    grobs[["gp"]] <- grid::gpar(alpha = 1)
    
    if (patternType %in% c("blank", "")) {
        return(do.call(grid::grobTree, args = grobs))
    }
    
    angle <- -angle
    angle <- (angle/180) * pi  ## converting angle from radians to degrees
    phaseDiff <- pi/6
    
    for (a in angle) {
        ## x/y coordinates of center line segment
        centralLineX <- c(radius * cos(a) + 0.5, -radius * cos(a) + 0.5)
        centralLineY <- c(radius * sin(a) + 0.5, -radius * sin(a) + 0.5)
        grobs[[length(grobs) + 1]] <- grid::linesGrob(x = centralLineX, 
            y = centralLineY, gp = linePar)
        
        ## x/y coordinates of left line segment
        leftLineX <- c(radius * cos(a + phaseDiff) + 0.5, -radius * 
            cos(a - phaseDiff) + 0.5)
        leftLineY <- c(radius * sin(a + phaseDiff) + 0.5, -radius * 
            sin(a - phaseDiff) + 0.5)
        grobs[[length(grobs) + 1]] <- grid::linesGrob(x = leftLineX, 
            y = leftLineY, gp = linePar)
        
        ## x/y coordinates of right line segment
        rightLineX <- c(radius * cos(a - phaseDiff) + 0.5, -radius * 
            cos(a + phaseDiff) + 0.5)
        rightLineY <- c(radius * sin(a - phaseDiff) + 0.5, -radius * 
            sin(a + phaseDiff) + 0.5)
        grobs[[length(grobs) + 1]] <- grid::linesGrob(x = rightLineX, 
            y = rightLineY, gp = linePar)
    }
    
    return(do.call(grid::grobTree, args = grobs))
}

#' Creates custom ggplot2 object
#' ggplot2 object that transfers the 
#' pattern aesthetics (e.g. color, lineColor, etc.) to render the legend icon.
#' @return List of grob objects
#' @noRd

imagePoints <- ggplot2::ggproto("imagePoints", ggplot2::Geom,
    required_aes=c("x", "y", "ids"),
    default_aes=ggplot2::aes(size = 5, shape=19),
    
    draw_key=function(data, params, size){
    ## necessary info to render icon
    iconInfo <- data$ids[[1]]
    legendIcon(iconInfo[[1]], iconInfo[[2]], iconInfo[[3]],
        iconInfo[[4]], iconInfo[[5]], iconInfo[[7]])
    },
    
    draw_group=function(data, panel_scales, coord){
        coords <- coord$transform(data, panel_scales)
        grid::pointsGrob(coords$x,coords$y, 
            grid::unit(0, "char"),pch=coords$shape, 
            gp=grid::gpar(col="black"))
    }
)

#' Creates custom ggplot2 layer that transfers the pattern
#' aesthetics (e.g. color, lineColor, etc.) to render the legend icon.
#' @param mapping Set of aesthetic mappings
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment, either as a string, or the result of
#' @param na.rm logical.  Should missing data (NA) be removed?
#' @param show.legend logical. Should this layer be included in the legends?
#' a call to a position adjustment function.
#' @param inherit.aes If FALSE, overrides the default aesthetics,
#' rather than combining with them.
#' @param ...  Additional parameters that may be passed to ggplot2::layer()
#' @return List of grob objects
#' @noRd
geom_imagePoint <- function(mapping=NULL, data=NULL, stat="identity",
    position="identity", na.rm=FALSE, 
    show.legend=NA, inherit.aes=TRUE, ...) {
    ggplot2::layer(
        geom = imagePoints, mapping=mapping,  data=data, stat=stat,
        position=position, show.legend=show.legend, inherit.aes=inherit.aes,
        params=list(na.rm = na.rm, ...)
    )
}
