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
#' @param rotatedxRange Rotated x-coordinate range of the plot.
#' @param rotatedyRange Rotated y-coordinate range of the plot.
#' @param sparsePoints Logical Vector denoting points annotated as sparse.  
#' If NULL, default sparsity detector will be used.
#' @return Dataframe defining the coordinates of each line to be drawn.
#' @noRd

drawHorizontal <- function(gridOutput, gridSize=NULL, pointSize,  
    rotatedxRange, rotatedyRange, sparsePoints=NULL){
    lineEndPoints <- data.frame(xStart=NULL,xEnd=NULL,yStart=NULL,yEnd=NULL)
    plotLargeClusters <- plotSparsePoints <- plotSmallClusters <- 1 
    
    xBins <- gridOutput[[1]]; yBins <- gridOutput[[2]]
    freqMat <- gridOutput[[3]]; pointsToGrid <- gridOutput[[4]]
    
    pointClassification <- getIrregularPoints(pointsToGrid, freqMat, sparsePoints, 
        rotatedxRange, rotatedyRange, pointSize)
    sparsePointsToGrid <- pointClassification[[1]] 
    smallClusterToGrid <- pointClassification[[2]]
    pointsToGrid <- pointClassification[[3]]; freqMat <- pointClassification[[4]]
    
    if (any(dim(smallClusterToGrid)==0)) plotSmallClusters <- 0
    if (any(dim(sparsePointsToGrid)==0)) plotSparsePoints <- 0
    if (any(dim(pointsToGrid)==0)) plotLargeClusters <- 0
    
    if (plotLargeClusters){
        ## dealing with large clusters
        lineEndPoints <- regularPatternDraw(freqMat, pointsToGrid, yBins)
    }
    
    
    ## dealing with sparse points
    if (plotSparsePoints){
        sparsePointEnds <- sparsePointsToGrid[,c('x','x','y','y')]
        names(sparsePointEnds) <- c("xStart","xEnd","yStart","yEnd")
        lineEndPoints <- rbind(lineEndPoints, sparsePointEnds)
    }
    
    ## dealing with small cluster points
    if (plotSmallClusters){
        gridSize <- gridSize/5
        smallClusterToGrid <- countGridPoints(smallClusterToGrid$x, 
            smallClusterToGrid$y, gridSize)[[4]]
        ## creating a unique identifier for each grid
        smallClusterToGrid$gridNum <- (smallClusterToGrid$yIntervals - 1)/max(smallClusterToGrid$yIntervals) + smallClusterToGrid$xIntervals
        
        for (gridNum in unique(smallClusterToGrid$gridNum)){
            xRange <- smallClusterToGrid$x[smallClusterToGrid$gridNum == gridNum]
            yRange <- smallClusterToGrid$y[smallClusterToGrid$gridNum == gridNum]
            clusterEnds <- data.frame(xStart=min(xRange),xEnd=max(xRange),yStart=min(yRange),yEnd=max(yRange))
            lineEndPoints <- rbind(lineEndPoints,clusterEnds)
        }
    }
    return(lineEndPoints)
}
