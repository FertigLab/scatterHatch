# Determines sparse points in a group
# Tejas Guha - January 1, 2021

#' Determines sparse points in a group
#'
#' Determines if points within a given group are sparse
#' based on the average distance from its ten closest points
#' and the a multiple of the diameter of a point.
#'
#' @param pointsToGrid Dataframe specifyingevery point in a group and the grid it belongs to
#' @param pointSize Point size of the plot.
#' @param xDiff x-coordinate range of the plot.
#' @param yDiff y-coordinate range of the plot.
#' @param scale If the sparsity of the pattern calculated is dependent which axes
#' @return Dataframe of each point in a group, which grid it belongs to, distance from fifth nearest point, and whether a point is sparse or not.
#' @export
sparsityDistanceCalc <- function(pointsToGrid, pointSize, xDiff, yDiff, scale){
  if (scale == 'x'){ pointRadius = abs(convertSizeToCartesian(pointSize, xDiff, 'x'))}
  if (scale == 'y'){ pointRadius = abs(convertSizeToCartesian(pointSize, yDiff, 'y'))}
  sparsityDistance = pointRadius *  1.75 # 3/4 a point away
  pointsToGrid$closest2Points = spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 2) # distance from the second closest point
  pointsToGrid$closest5Points = spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 5) # distance from the fifth closest point
  pointsToGrid$closest20Points = spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 20) # distance from the thirtieth closest point
  pointsToGrid$outliers = pointsToGrid$closest5Points > sparsityDistance


  pointsToGrid$smallClusters = (pointsToGrid$closest2Points < pointRadius) & (pointsToGrid$closest20Points > pointRadius*4)
  pointsToGrid$sparsePoints = pointsToGrid$outliers | pointsToGrid$smallClusters

  #sparseClusterMinDistance = as.double(quantile(pointsToGrid$closest20Points, 0.995))
  # print(table(pointsToGrid$sparsePoints))
  #quantile method doesn't work

  # identifying sparse cluster points

  # pointsToGrid$closestFivePoints = spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 5)
  # pointsToGrid$closestTwentyPoints = spatstat::nndist(pointsToGrid$x, pointsToGrid$y, k = 20)
  # pointsToGrid$smallClusters = (pointsToGrid$closestFivePoints <= pointRadius) &
  #   (pointsToGrid$closestTwentyPoints >= pointRadius * 4) &
  #   (pointsToGrid$yIntervals %% as.integer(1/density) != 0)

  return(pointsToGrid)
}


