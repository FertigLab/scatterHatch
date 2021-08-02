# Draws the horizontal line pattern
# Tejas Guha - September 13, 2020

#' Draws the horizontal line pattern
#'
#' Using the 2D frequency matrix from countGridPoints(), this function 
#' goes by row checking if each grid contains a point.  The function records 
#' the leftmost and rightmost points in a consecutive sequence of grids with 
#' points.  The function also records points in a sparse grid: defined
#' as being a grid where grids n-rows or n-columns away do not contain any 
#' points.
#'
#' @param gridOutput List: x & y bins of the grid, 2D frequency matrix, and 
#' dataframe matching each point to its corresponding grid in the matrix
#' @param density Density of the pattern to be drawn.  Must be 1 or less.  
#' Skips drawing the pattern every n rows.
#' @param pointSize Point size of the pattern.
#' @param xRange x-coordinate range of the plot.
#' @param yRange y-coordinate range of the plot.
#' @param rotatedxRange Rotated x-coordinate range of the plot.
#' @param rotatedyRange Rotated y-coordinate range of the plot.
#' @param sparsePoints Logical Vector denoting points annotated as sparse.  
#' If NULL, default sparsity detector will be used.
#' @return Dataframe defining the coordinates of each line to be drawn.
#' @noRd

drawHorizontal <- function(gridOutput, density=1, pointSize, xRange, yRange, 
                           rotatedxRange, rotatedyRange, sparsePoints=NULL){
    xBins <- gridOutput[[1]]; yBins <- gridOutput[[2]]
    freqMat <- gridOutput[[3]]; pointsToGrid <- gridOutput[[4]]
  
    output <- getIrregularPoints(pointsToGrid, freqMat, sparsePoints, 
                               rotatedxRange, rotatedyRange, xRange, pointSize)
    sparsePointsToGrid <- output[[1]]; smallClusterToGrid <- output[[2]]
    pointsToGrid <- output[[3]]; freqMat <- output[[4]]
  
    ## dealing with large clusters
    output <- regularPatternDraw(freqMat, pointsToGrid, yBins, density)
    xStart <- output[[1]]; xEnd <- output[[2]] 
    yStart <- output[[3]]; yEnd <- output[[4]]
  
    ## dealing with sparse points
    xStart <- c(xStart, sparsePointsToGrid$x)
    xEnd <- c(xEnd, sparsePointsToGrid$x)
    yStart <- c(yStart, sparsePointsToGrid$y)
    yEnd <- c(yEnd, sparsePointsToGrid$y)
  
    ## dealing with small cluster points
    pointRadius <- convertSizeToCartesian(pointSize, xRange, 'x')
    smallClusterGridSize <- as.integer(diff(rotatedxRange)/(pointRadius * 5))
    smallClusterToGrid <- countGridPoints(smallClusterToGrid$x, 
                                       smallClusterToGrid$y, rotatedxRange, 
                                       rotatedyRange, smallClusterGridSize)[[4]]
    smallClusterToGrid$gridNum <- (smallClusterToGrid$yIntervals - 1) * 
                            smallClusterGridSize + smallClusterToGrid$xIntervals
  
    for (gridNum in unique(smallClusterToGrid$gridNum)){
        xRange <- smallClusterToGrid$x[smallClusterToGrid$gridNum == gridNum]
        yRange <- smallClusterToGrid$y[smallClusterToGrid$gridNum == gridNum]
        xStart <- c(xStart, min(xRange))
        xEnd <- c(xEnd, max(xRange))
        yStart <- c(yStart, median(yRange))
        yEnd <- c(yEnd, median(yRange))
    }

    return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}
