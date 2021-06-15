# Determines sparse points in a group
# Tejas Guha - January 1, 2021

#' Determines sparse points in a group
#'
#' Determines which points within a given group are sparse
#' and which are in small clusters.  Outlying points are determined if
#' fifth closest point is a point away.  Small clusters are determined
#' if twentieth closest point is 2 points away.
#'
#' @param pointsToGrid Dataframe specifying every point in a group and the grid it belongs to
#' @param pointSize Point size of the plot.
#' @param xDiff x-coordinate range of the plot.
#' @param yDiff y-coordinate range of the plot.
#' @param scale If the sparsity of the pattern calculated is dependent which axes
#' @return Dataframe of each point in a group, which grid it belongs to, distance from fifth nearest point, and whether a point is sparse or not.
#' @export
#' @importFrom spatstat.geom nndist
sparsityAnnotate <- function(pointsToGrid, pointSize, xDiff, yDiff, scale){
  if (scale == 'x'){ pointRadius = abs(convertSizeToCartesian(pointSize, xDiff, 'x'))}
  if (scale == 'y'){ pointRadius = abs(convertSizeToCartesian(pointSize, yDiff, 'y'))}
  sparsityDistance = pointRadius * 2  # a point away
  pointsToGrid$closest2Points = suppressMessages(nndist(pointsToGrid$x, pointsToGrid$y, k = 2)) # distance from the second closest point
  pointsToGrid$closest5Points = suppressMessages(nndist(pointsToGrid$x, pointsToGrid$y, k = 5)) # distance from the fifth closest point
  pointsToGrid$closest20Points = suppressMessages(nndist(pointsToGrid$x, pointsToGrid$y, k = 20)) # distance from the twentieth closest point
  pointsToGrid$sparsePoints = pointsToGrid$closest5Points > sparsityDistance # outlying points within a group

  pointsToGrid$smallClusters = (pointsToGrid$closest20Points > pointRadius*4) & !pointsToGrid$sparsePoints # smaller clusters within a group

  return(pointsToGrid)
}


