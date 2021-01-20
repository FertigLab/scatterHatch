# Determines sparse points in a group
# Tejas Guha - January 1, 2021

#' Determines sparse points in a group
#'
#' Determines if points within a given group are sparse
#' based on the average distance from its ten closest points
#' and the a multiple of the diameter of a point.
#'
#' @param pointsToGrid Dataframe specifying every point in a group and the grid it belongs to
#' @param pointSize Point size of the plot.
#' @param xDiff x-coordinate range of the plot.
#' @param yDiff y-coordinate range of the plot.
#' @param scale If the sparsity of the pattern calculated is dependent which axes
#' @return Dataframe of each point in a group, which grid it belongs to, distance from fifth nearest point, and whether a point is sparse or not.
#' @export
sparsityAnnotate <- function(pointsToGrid, pointSize, xDiff, yDiff, scale){
  if (scale == 'x'){ pointRadius = abs(convertSizeToCartesian(pointSize, xDiff, 'x'))}
  if (scale == 'y'){ pointRadius = abs(convertSizeToCartesian(pointSize, yDiff, 'y'))}
  sparsityDistance = pointRadius *  1.75 # 3/4 a point away
  pointsToGrid$closest2Points = suppressMessages(spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 2)) # distance from the second closest point
  pointsToGrid$closest5Points = suppressMessages(spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 5)) # distance from the fifth closest point
  pointsToGrid$closest20Points = suppressMessages(spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 20)) # distance from the twentieth closest point
  pointsToGrid$sparsePoints = pointsToGrid$closest5Points > sparsityDistance


  pointsToGrid$smallClusters = (pointsToGrid$closest20Points > pointRadius*4) & !pointsToGrid$sparsePoints


  #pointsToGrid$sparsePoints = pointsToGrid$outliers | pointsToGrid$smallClusters


  return(pointsToGrid)
}


