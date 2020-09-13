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
#' @param xDiff Range of the x-axis in the scatterplot
#' @param yDiff Range of the y-axis in the scatterplot
#' @param n Size of the square matrix
#' @return List: x & y bins of the grid, 2D frequency matrix, and dataframe matching each point to its corresponding grid in the matrix
#' @export
countGridPoints <- function(x, y, xDiff, yDiff, n = 25){
  xIntervals = as.numeric(cut(x, breaks = seq(xDiff[1], xDiff[2], by=diff(xDiff)/(n-1))))
  yIntervals = as.numeric(cut(y, breaks = seq(yDiff[2], yDiff[1], by=-1*diff(yDiff)/(n-1))))
  yIntervals = n+1-yIntervals
  intervals = as.data.frame(cbind(xIntervals, yIntervals))
  pointsToGrid = as.data.frame(cbind(xIntervals, yIntervals, x, y))
  freqs = plyr::count(intervals, vars=c("xIntervals", "yIntervals"))
  freqMat = matrix(0, nrow=n, ncol=n)
  for (x in 1:nrow(freqs)){
    freqMat[freqs$yIntervals[x], freqs$xIntervals[x]] = freqs$freq[x]
  }

  xLevels = seq(xDiff[1], xDiff[2], by=diff(xDiff)/(n-1))
  yLevels = seq(yDiff[2], yDiff[1], by=-1*diff(yDiff)/(n-1))
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
#' @param scale Minimum and maximum values of given scale
#' @param axis Cartesian axis: 'x' or 'y'
#' @return Radius of the point in Cartesian units
#' @export
convertSizeToCartesian <- function(size, scale, axis){
  fontSize = size*ggplot2::.pt + ggplot2::.stroke*0.5/2
  if (axis == 'x'){
    cartesianConvert = grid::convertWidth(grid::unit(fontSize, "points"), unitTo="npc", valueOnly = TRUE) * diff(scale)/2.440651
  }
  if (axis == 'y'){
    cartesianConvert = grid::convertHeight(grid::unit(fontSize, "points"), unitTo="npc", valueOnly = TRUE) * diff(scale)/2.440651
  }
  return(cartesianConvert)
}
