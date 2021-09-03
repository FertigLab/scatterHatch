# Preparing helper functions to create each pattern
# Tejas Guha - September 13, 2020

#' Creates a 2D frequency matrix
#'
#' This function creates a matrix of size n by n.  Each grid of the
#' matrix corresponds to a range of x coordinates and y coordinates
#' in the scatterplot.  The amount of points from a specific group
#' in each grid are counted.  Increasing the grid size, or n, increases
#' the precision of the patterns.
#'
#' @param x x-coordinates of the group
#' @param y y-coordinates of the group
#' @param gridSize width of pattern grid
#' @return List: x & y bins of the grid, 2D frequency matrix, and 
#' dataframe matching each point to its corresponding grid in the matrix
#' @noRd
countGridPoints <- function(x, y, gridSize){
    xgridStep <- gridSize
    xRange = c(min(x)-0.5*xgridStep,max(x)+xgridStep)
    yRange = c(min(y)-0.5*gridSize,max(y)+gridSize)
    xIntervals <- as.numeric(cut(x, breaks=seq(xRange[1], xRange[2], 
        by=xgridStep)))
    yIntervals <- as.numeric(cut(y, breaks=seq(yRange[1], yRange[2], 
        by=1*gridSize)))
    
    ## ensures as row index increases, y decreases in matrix
    #yIntervals <- ceiling(diff(yRange)/gridSize)-yIntervals
    intervals <- as.data.frame(cbind(xIntervals, yIntervals))
    pointsToGrid <- as.data.frame(cbind(xIntervals, yIntervals, x, y))
    freqs <- plyr::count(intervals, vars=c("xIntervals", "yIntervals"))
    freqMat <- matrix(0, nrow=max(yIntervals), ncol=max(xIntervals))
    for (ind in seq(nrow(freqs))){
        freqMat[freqs$yIntervals[ind], freqs$xIntervals[ind]] <- freqs$freq[ind]
    }
    
    xLevels <- seq(xRange[1], xRange[2], by=xgridStep)
    yLevels <- seq(yRange[1], yRange[2], by=1*gridSize)
    return(list(xLevels, yLevels, freqMat, pointsToGrid))
}

#' Converts a point size to Cartesian units
#'
#' Given a point size and the scale of an axis, this function is able to
#' convert the radius of the pointGrob being created to its width in
#' Cartesian units.  This function is used to ensure the patterns extend
#' a whole point or region no matter the point size.
#'
#' @param size Size of the pointGrob
#' @param dimRange Minimum and maximum values of given scale
#' @param whichAxis Cartesian axis: 'x' or 'y'
#' @return Radius of the point in Cartesian units
#' @noRd
convertSizeToCartesian <- function(size, dimRange, whichAxis){
    #fontSize = size*ggplot2::.pt + ggplot2::.stroke*0.5/2
    fontSize <- size*ggplot2::.pt
    aspectRatio <- dev.size()[1]/dev.size()[2]
    if (aspectRatio >= 1){
        if (whichAxis == 'x'){
            cartesianConvert <- grid::convertWidth(
                grid::unit(fontSize, "points"), unitTo="npc", valueOnly=TRUE) * 
                diff(dimRange)/aspectRatio
        }
        if (whichAxis == 'y'){
            cartesianConvert <- grid::convertHeight(
                grid::unit(fontSize, "points"), unitTo="npc", valueOnly=TRUE) * 
                diff(dimRange)/aspectRatio
        }
    }
    
    if (aspectRatio < 1){
        if (whichAxis == 'x'){
            cartesianConvert <- grid::convertWidth(
                grid::unit(fontSize, "points"), unitTo="npc", valueOnly=TRUE) * 
                diff(dimRange)*aspectRatio
        }
        if (whichAxis == 'y'){
            cartesianConvert <- grid::convertHeight(
                grid::unit(fontSize, "points"), unitTo="npc", valueOnly=TRUE) * 
                diff(dimRange)*aspectRatio
        }
    }
    
    return(cartesianConvert/2)
}
