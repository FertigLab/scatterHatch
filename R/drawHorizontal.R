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
#' @param gridSize pattern grid size to be drawn
#' @param pointSize Point size of the pattern.
#' @param xRange x-coordinate range of the plot.
#' @param yRange y-coordinate range of the plot.
#' @param rotatedxRange Rotated x-coordinate range of the plot.
#' @param rotatedyRange Rotated y-coordinate range of the plot.
#' @param sparsePoints Logical Vector denoting points annotated as sparse.  
#' If NULL, default sparsity detector will be used.
#' @return Dataframe defining the coordinates of each line to be drawn.
#' @noRd

drawHorizontal <- function(gridOutput, gridSize=NULL, pointSize, xRange, yRange, 
                           rotatedxRange, rotatedyRange, sparsePoints=NULL){
    xStart <- xEnd <- yStart <- yEnd <- c(); 
    plotLargeClusters <- 1; plotSparsePoints <-1; plotSmallClusters <- 1; plotSmallClustersAsSparse <-0
    
    xBins <- gridOutput[[1]]; yBins <- gridOutput[[2]]
    freqMat <- gridOutput[[3]]; pointsToGrid <- gridOutput[[4]]
  
    pointClassification <- getIrregularPoints(pointsToGrid, freqMat, sparsePoints, 
                               rotatedxRange, rotatedyRange, xRange, pointSize)
    sparsePointsToGrid <- pointClassification[[1]]; smallClusterToGrid <- pointClassification[[2]]
    pointsToGrid <- pointClassification[[3]]; freqMat <- pointClassification[[4]]
  
    if (plotLargeClusters){
      ## dealing with large clusters
      endPoints <- regularPatternDraw(freqMat, pointsToGrid, yBins)
      xStart <- c(xStart,endPoints[[1]]); xEnd <- c(xEnd,endPoints[[2]])
      yStart <- c(yStart,endPoints[[3]]); yEnd <- c(yEnd,endPoints[[4]])  
    }
    
  
    ## dealing with sparse points
    if (plotSparsePoints){
      xStart <- c(xStart, sparsePointsToGrid$x)
      xEnd <- c(xEnd, sparsePointsToGrid$x)
      yStart <- c(yStart, sparsePointsToGrid$y)
      yEnd <- c(yEnd, sparsePointsToGrid$y)  
    }
    
    ## dealing with small cluster points
    if (plotSmallClusters){
      smallClusterGridSize <- gridSize
      smallClusterToGrid <- countGridPoints(smallClusterToGrid$x, 
                                            smallClusterToGrid$y, smallClusterGridSize)[[4]]
      ## creating a unique identifier for each grid
      smallClusterToGrid$gridNum <- (smallClusterToGrid$yIntervals - 1)/max(smallClusterToGrid$yIntervals) + smallClusterToGrid$xIntervals
      
      for (gridNum in unique(smallClusterToGrid$gridNum)){
        xRange <- smallClusterToGrid$x[smallClusterToGrid$gridNum == gridNum]
        yRange <- smallClusterToGrid$y[smallClusterToGrid$gridNum == gridNum]
        xStart <- c(xStart, min(xRange))
        xEnd <- c(xEnd, max(xRange))
        yStart <- c(yStart, median(yRange))
        yEnd <- c(yEnd, median(yRange))
      }
    } else if (plotSmallClustersAsSparse){
        xStart <- c(xStart, smallClusterToGrid$x)
        xEnd <- c(xEnd, smallClusterToGrid$x)
        yStart <- c(yStart, smallClusterToGrid$y)
        yEnd <- c(yEnd, smallClusterToGrid$y)  
      }
    
    return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}
