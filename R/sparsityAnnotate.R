# Determines sparse points in a group Tejas Guha - January 1, 2021

#' Determines sparse points in a group
#'
#' Determines which points within a given group are sparse
#' and which are in small clusters.  Outlying points are determined if
#' fifth closest point is a point away.  Small clusters are determined
#' if twentieth closest point is 2 points away.
#'
#' @param pointsToGrid Dataframe of points in group and the grid it belongs to
#' @param pointSize Point size of the plot.
#' @param xRange x-coordinate range of the plot.
#' @param yRange y-coordinate range of the plot.
#' @param scale Sparsity dependent on which axes?
#' @return Dataframe determining sparsity of a point.
#' @noRd
#' @importFrom spatstat.geom nndist
sparsityAnnotate <- function(pointsToGrid, pointSize, xRange, yRange, whichAxis) {
    
    if (whichAxis == "x") {
        pointRadius <- abs(convertSizeToCartesian(max(pointSize,1), xRange, "x"))
    }
    if (whichAxis == "y") {
        pointRadius <- abs(convertSizeToCartesian(max(pointSize,1), yRange, "y"))
    }
    
    sparsityDistance <- pointRadius * 2  # a point away
    
    ## distance from the second closest point
    pointsToGrid$closest2Points <- suppressMessages(nndist(pointsToGrid$x, 
        pointsToGrid$y, k = 2))
    
    ## distance from the fifth closest point
    pointsToGrid$closest5Points <- suppressMessages(nndist(pointsToGrid$x, 
        pointsToGrid$y, k = 5))
    
    ## distance from the twentieth closest point
    pointsToGrid$closest20Points <- suppressMessages(nndist(pointsToGrid$x, 
        pointsToGrid$y, k = 20))
    
    ## outlying points within a group
    pointsToGrid$sparsePoints <- pointsToGrid$closest2Points > sparsityDistance
    
    ## smaller clusters within a group
    pointsToGrid$smallClusters <- (pointsToGrid$closest5Points>pointRadius*4) & 
        !pointsToGrid$sparsePoints
    return(pointsToGrid)
}


