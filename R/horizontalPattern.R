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
#' @export

drawHorizontal <- function(gridOutput, density = 1/3, pointSize, xDiff, yDiff, rotatedxDiff, rotatedyDiff, sparsePoints = NULL){
  if (density > 1){density=1} # density must be 1 or less
  xBins = gridOutput[[1]]
  yBins = gridOutput[[2]]
  freqMat = gridOutput[[3]]
  pointsToGrid = gridOutput[[4]]

  if (is.null(sparsePoints)){ # if sparse points are not given
    sparsePoints = sparsityAnnotate(pointsToGrid, pointSize, rotatedxDiff, rotatedyDiff, 'x')$sparsePoints # annotates sparse points
  }

  sparsePointsToGrid = pointsToGrid[sparsePoints == TRUE, ]
  pointsToGrid = pointsToGrid[sparsePoints == FALSE, ] # removes sparse points from regular pattern drawing

  # removes sparse points from 2D frequency matrix
  # freqMat = sapply(1:(nrow(freqMat)^2), function (i){
  #   row = ceiling(i/nrow(freqMat))
  #   col = i - (row * nrow(freqMat))
  #   freqMat[row, col] = freqMat[row, col] - sum(sparsePointsToGrid$yIntervals == row & sparsePointsToGrid$xIntervals == col)})

  for (i in 1:nrow(sparsePointsToGrid)){ # removes sparse points from 2D frequency matrix
    freqMat[sparsePointsToGrid$yIntervals[i], sparsePointsToGrid$xIntervals[i]] = freqMat[sparsePointsToGrid$yIntervals[i], sparsePointsToGrid$xIntervals[i]] - 1
  }

  xStart = c()
  yStart = c()
  xEnd = c()
  yEnd = c()
  adjustmentFactorX = convertSizeToCartesian(pointSize, rotatedxDiff, 'x')
  adjustmentFactorY = convertSizeToCartesian(pointSize, rotatedyDiff, 'y')

  rowDraw = TRUE # whether to draw lines in current row or not
  for (row in 1:nrow(freqMat)){ # iterates by every row
    currentRow = freqMat[row, ]
    rowPoints = pointsToGrid[pointsToGrid$yIntervals == row, ]

    yLevels = yBins[row] - (yBins[1] - yBins[2])/2 # where to draw y level
    if (row == nrow(freqMat)){ # for bottom row exception
      yLevels = yBins[row]
    }

    rowDraw = (as.integer(row * density)*(1/density)) == row # when to skip rows

    prevCol = 0
    lineDraw = FALSE # whether line being drawn or not


    for (col in 1:ncol(freqMat)){
      if (!lineDraw & prevCol == 0 & freqMat[row, col] != 0  & rowDraw){ # starting a line segment
        gridPoints = rowPoints[rowPoints$xIntervals == col, ] # points corresponding to current grid square
        xStart = c(xStart, min(gridPoints$x) - adjustmentFactorX) # where to draw horizontal lines
        yStart = c(yStart, yLevels)
        lineDraw = TRUE
      }

      if (lineDraw & currentRow[col] == 0 & rowDraw){ # ending line segment
        gridPoints = rowPoints[rowPoints$xIntervals == col-1, ] # points corresponding to current grid square

        xEnd = c(xEnd, max(gridPoints$x) + adjustmentFactorX) # where to draw horizontal lines
        yEnd = c(yEnd, yLevels)
        lineDraw = FALSE

      }

      prevCol = currentRow[col]
    }

  }

  # dealing with sparse points
  sparseGridSize = as.integer(diff(rotatedxDiff)/(adjustmentFactorX * 5))
  sparsePointsToGrid = countGridPoints(sparsePointsToGrid$x, sparsePointsToGrid$y, rotatedxDiff, rotatedyDiff, sparseGridSize)[[4]]
  sparsePointsToGrid$gridNum = (sparsePointsToGrid$yIntervals - 1) * sparseGridSize + sparsePointsToGrid$xIntervals
  for (gridNum in unique(sparsePointsToGrid$gridNum)){
    xRange = sparsePointsToGrid$x[sparsePointsToGrid$gridNum == gridNum]
    yRange = sparsePointsToGrid$y[sparsePointsToGrid$gridNum == gridNum]
    xStart = c(xStart, min(xRange) - adjustmentFactorX)
    xEnd = c(xEnd, max(xRange) + adjustmentFactorX)
    yStart = c(yStart, median(yRange))
    yEnd = c(yEnd, median(yRange))
  }


  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}
