# Draws the vertical line pattern
# Tejas Guha - September 13, 2020

#' Draws the vertical line pattern
#'
#' Using the 2D frequency matrix from countGridPoints(), this function goes column by column
#' checking if each grid contains a point.  The function records the lowest and highest points in a
#' consecutive sequence of grids with points.  The function also records points in a sparse grid: defined
#' as being a grid where grids n-rows or n-columns away do not contain any points.
#'
#' @param gridOutput List: x & y bins of the grid, 2D frequency matrix, and dataframe matching each point to its corresponding grid in the matrix
#' @param density Density of the pattern to be drawn.  Must be 1 or less.  Skips drawing the pattern every n columns.
#' @param sparsity Threshold for defining a sparse grid.
#' @param size Point size of the pattern.
#' @param xDiff x-coordinate range of the plot.
#' @param yDiff y-coordinate range of the plot.
#' @return Dataframe with 4 columns defining the starting and ending coordinates of each line to be drawn.
#' @export

drawVertical <- function(gridOutput, density = 1/2, sparsity = 5, size = 1, xDiff, yDiff){
  if (density > 1){density=1} # must be 1 or less
  xBins = gridOutput[[1]]
  yBins = gridOutput[[2]]
  freqMat = gridOutput[[3]]
  pointsToGrid = gridOutput[[4]]
  xStart = c()
  yStart = c()
  xEnd = c()
  yEnd = c()
  adjustmentFactorX = convertSizeToCartesian(size, xDiff, 'x')
  adjustmentFactorY = convertSizeToCartesian(size, yDiff, 'y')

  colDraw = TRUE # wheater to draw lines in current col or not
  for (col in 1:ncol(freqMat)){ # iterates by every row
    currentCol = freqMat[, col]
    colPoints = pointsToGrid[pointsToGrid$xIntervals == col, ]

    if (col+1 <= ncol(freqMat)){  # where to draw x levels
      xLevels = median(colPoints$x)
    }

    colDraw = (as.integer(col * density)*(1/density)) == col

    if (col + 1 > ncol(freqMat)){ # for top rows exception case
      xLevels = xBins[col]
    }

    prevRow = 0
    lineDraw = FALSE # wheather line being drawn or not


    for (row in 1:nrow(freqMat)){
      if (!lineDraw & prevRow == 0 & currentCol[row] != 0 & colDraw){ # starting a line segment
        gridPoints = colPoints[colPoints$yIntervals == row, ] # points corresponding to current grid square
        xStart = c(xStart, xLevels) # where to draw vertical lines
        yStart = c(yStart, max(gridPoints$y) + adjustmentFactorY)
        lineDraw = TRUE
      }

      if (lineDraw & currentCol[row] == 0 & colDraw){ # ending line segment
        gridPoints = colPoints[colPoints$yIntervals == row-1, ] # points corresponding to previous grid
        xEnd = c(xEnd, xLevels) # where to draw vertical lines
        yEnd = c(yEnd, min(gridPoints$y) - adjustmentFactorY)
        lineDraw = FALSE
      }

      # dealing with sparse points
      surroundingGrid = c((row-sparsity):(row-1), (row+1):(row+sparsity))
      surroundingGrid = surroundingGrid[surroundingGrid > 0 & surroundingGrid < nrow(freqMat)] # grids sparsity horizontal distance away
      if (sum(currentCol[surroundingGrid]) <= 0 & currentCol[row] != 0 & !lineDraw){
        gridPoints = colPoints[colPoints$yIntervals == row, ]
        xStart = c(xStart, median(gridPoints$x)) # where to draw horizontal lines
        yStart = c(yStart, min(gridPoints$y) - adjustmentFactorY)
        xEnd = c(xEnd, median(gridPoints$x)) # where to draw horizontal lines
        yEnd = c(yEnd, max(gridPoints$y) + adjustmentFactorY)
      }

      prevRow = currentCol[row]
    }
  }

  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}
