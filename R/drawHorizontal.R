# Draws the horizontal line pattern
# Tejas Guha - September 13, 2020

#' Draws the horizontal line pattern
#'
#' Using the 2D frequency matrix from countGridPoints(), this function goes row by row
#' checking if each grid contains a point.  The function records the leftmost and rightmost points in a
#' consecutive sequence of grids with points.  The function also records points in a sparse grid: defined
#' as being a grid where grids n-rows or n-columns away do not contain any points.
#'
#' @param gridOutput List: x & y bins of the grid, 2D frequency matrix, and dataframe matching each point to its corresponding grid in the matrix
#' @param density Density of the pattern to be drawn.  Must be 1 or less.  Skips drawing the pattern every n rows.
#' @param pointSize Point size of the pattern.
#' @param xDiff x-coordinate range of the plot.
#' @param yDiff y-coordinate range of the plot.
#' @param rotatedxDiff Rotated x-coordinate range of the plot.
#' @param rotatedyDiff Rotated y-coordinate range of the plot.
#' @param sparsePoints Logical Vector denoting points annotated as sparse.  If NULL, default sparsity detector will be used to annotate sparse points in dataset.
#' @return Dataframe with 4 columns defining the starting and ending coordinates of each line to be drawn.
#' @noRd

drawHorizontal <- function(gridOutput, density = NULL, pointSize, xDiff, yDiff, rotatedxDiff, rotatedyDiff, sparsePoints = NULL){
  if (density > 1){density=1} # density must be 1 or less
  #density = 1;
  xBins = gridOutput[[1]]
  yBins = gridOutput[[2]]
  freqMat = gridOutput[[3]]
  pointsToGrid = gridOutput[[4]]
  sparsityAnnotateOutput = sparsityAnnotate(pointsToGrid, pointSize, rotatedxDiff, rotatedyDiff, 'x') # annotates sparse points and small clusters
  
  if (is.null(sparsePoints)){ # if sparse points are not given
    sparsePoints = sparsityAnnotateOutput$sparsePoints
  }
  
  sparsePointsToGrid = pointsToGrid[sparsePoints, ]
  smallClusterToGrid = pointsToGrid[sparsityAnnotateOutput$smallClusters, ]
  
  pointsToGrid = pointsToGrid[!sparsePoints & !sparsityAnnotateOutput$smallClusters, ] # removes sparse and small cluster points from regular pattern drawing
  
  allIrregularPoints = rbind(sparsePointsToGrid, smallClusterToGrid)
  for (i in seq(nrow(allIrregularPoints))){ # removes sparse points from 2D frequency matrix
    freqMat[allIrregularPoints$yIntervals[i], allIrregularPoints$xIntervals[i]] = freqMat[allIrregularPoints$yIntervals[i], allIrregularPoints$xIntervals[i]] - 1
  }
  
  xStart = c()
  yStart = c()
  xEnd = c()
  yEnd = c()
  
  rowDraw = TRUE # whether to draw lines in current row or not
  for (row in seq(nrow(freqMat))){ # iterates by every row
    rowPoints = pointsToGrid[pointsToGrid$yIntervals == row, ]
    
    #yLevels = yBins[row] - (yBins[1] - yBins[2])/2 # where to draw y level @Tejas's version
    yLevels = yBins[row] - diff(yBins)[1]/2 #atul's version
    if (row == nrow(freqMat)){ # for bottom row exception
      yLevels = yBins[row] - diff(yBins)[1]/2
    }
    
    rowDraw = (as.integer(row * density)*(1/density)) == row # when to skip rows
    
    prevCol = 0
    lineDraw = FALSE # whether line being drawn or not
    
    
    for (col in seq(ncol(freqMat))){
      if (prevCol == 0 & freqMat[row, col] != 0  & rowDraw){ # starting a line segment
        gridPoints = rowPoints[rowPoints$xIntervals == col, ] # points corresponding to current grid square
        xStart = c(xStart, min(gridPoints$x)) # where to draw horizontal lines
        yStart = c(yStart, yLevels)
        lineDraw = TRUE
      }
      
      if (lineDraw & freqMat[row, col] == 0 & rowDraw){ # ending line segment
        gridPoints = rowPoints[rowPoints$xIntervals == col-1, ] # points corresponding to current grid square
        
        xEnd = c(xEnd, max(gridPoints$x)) # where to draw horizontal lines
        yEnd = c(yEnd, yLevels)
        lineDraw = FALSE
        
      }
      
      prevCol = freqMat[row, col]
    }
    
  }
  
  
  # dealing with sparse points
  xStart = c(xStart, sparsePointsToGrid$x)
  xEnd = c(xEnd, sparsePointsToGrid$x)
  yStart = c(yStart, sparsePointsToGrid$y)
  yEnd = c(yEnd, sparsePointsToGrid$y)
  
  # dealing with small cluster points
  pointRadius = convertSizeToCartesian(pointSize, xDiff, 'x')
  smallClusterGridSize = as.integer(diff(rotatedxDiff)/(pointRadius * 5))
  smallClusterToGrid = countGridPoints(smallClusterToGrid$x, smallClusterToGrid$y, rotatedxDiff, rotatedyDiff, smallClusterGridSize)[[4]]
  smallClusterToGrid$gridNum = (smallClusterToGrid$yIntervals - 1) * smallClusterGridSize + smallClusterToGrid$xIntervals
  for (gridNum in unique(smallClusterToGrid$gridNum)){
    xRange = smallClusterToGrid$x[smallClusterToGrid$gridNum == gridNum]
    yRange = smallClusterToGrid$y[smallClusterToGrid$gridNum == gridNum]
    xStart = c(xStart, min(xRange))
    xEnd = c(xEnd, max(xRange))
    yStart = c(yStart, median(yRange))
    yEnd = c(yEnd, median(yRange))
  }



  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}
