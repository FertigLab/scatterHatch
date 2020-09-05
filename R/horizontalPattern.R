#Developed By Tejas Guha: guhatejas@gmail.com or tguha@umd.edu

#' Draws the horizontal line pattern
#'
#' Using the 2D frequency matrix from countGridPoints(), this function goes row by row
#' checking if each grid contains a point.  The function records the leftmost and rightmost points in a
#' consecutive sequence of grids with points.  The function also records points in a sparse grid: defined
#' as being a grid where grids n-rows or n-columns away do not contain any points.
#'
#' @param gridOutput List: x & y bins of the grid, 2D frequency matrix, and dataframe matching each point to its corresponding grid in the matrix
#' @param density Density of the pattern to be drawn.  Must be 1 or less.  Skips drawing the pattern every n rows.
#' @param sparsity Threshold for defining a sparse grid.
#' @param size Point size of the pattern.
#' @param xDiff x-coordinate range of the plot.
#' @param yDiff y-coordinate range of the plot.
#' @return Dataframe with 4 columns defining the starting and ending coordinates of each line to be drawn.
#' @export

drawHorizontal <- function(gridOutput, density = 1/2, sparsity = 5, size = 1, xDiff, yDiff){
  if (density > 1){density=1} #density must be 1 or less
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
  rowDraw = TRUE # wheater to draw lines in current row or not
  for (row in 1:nrow(freqMat)){ # iterates by every row
    currentRow = freqMat[row, ]
    rowPoints = pointsToGrid[pointsToGrid$yIntervals == row, ]

    if (density <= 1 & row+1 <= nrow(freqMat)){  # where to draw y levels if density < 1
      yLevels = median(rowPoints$y)
    }

    if (density <= 1){ # if density < 1, when to skip rows
      rowDraw = (as.integer(row * density)*(1/density)) == row
    }

    if (row + 1 > nrow(freqMat)){ # for top rows exception case
      yLevels = yBins[row]
    }

    prevCol = 0
    lineDraw = FALSE # wheather line being drawn or not


    for (col in 1:ncol(freqMat)){
      if (!lineDraw & prevCol == 0 & freqMat[row, col] != 0 & rowDraw){ # starting a line segment
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

      # dealing with sparse points
      surroundingGrid = c((col-sparsity):(col-1), (col+1):(col+sparsity))
      surroundingGrid = surroundingGrid[surroundingGrid > 0 & surroundingGrid < ncol(freqMat)] # grids sparsity horizontal distance away
      if (sum(currentRow[surroundingGrid]) <= 0 & currentRow[col] != 0 & !lineDraw){
        gridPoints = rowPoints[rowPoints$xIntervals == col, ]
        xStart = c(xStart, min(gridPoints$x) - adjustmentFactorX) # where to draw horizontal lines
        yStart = c(yStart, median(gridPoints$y))
        xEnd = c(xEnd, max(gridPoints$x) + adjustmentFactorX) # where to draw horizontal lines
        yEnd = c(yEnd, median(gridPoints$y))
      }

      prevCol = currentRow[col]
    }
  }

  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}
