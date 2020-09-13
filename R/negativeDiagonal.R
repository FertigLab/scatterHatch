# draws the negative diagonal pattern

#' Draws the negative diagonal pattern
#'
#' Using the 2D frequency matrix from countGridPoints(), this function goes diagonal by diagonal
#' checking if each grid contains a point.  The function records the lowest and highest points in a
#' consecutive sequence of grids with points.  The function also records points in a sparse grid: defined
#' as being a grid where grids n-rows or n-columns away do not contain any points.
#'
#' @param gridOutput List: x & y bins of the grid, 2D frequency matrix, and dataframe matching each point to its corresponding grid in the matrix
#' @param density Density of the pattern to be drawn.  Must be 1 or less.  Skips drawing the pattern every n diagonals.
#' @param sparsity Threshold for defining a sparse grid.
#' @param size Point size of the pattern.
#' @param xDiff x-coordinate range of the plot.
#' @param yDiff y-coordinate range of the plot.
#' @return Dataframe with 4 columns defining the starting and ending coordinates of each line to be drawn.
#' @export

drawNegativeDiagonal <- function(gridOutput, density = 1/2, sparsity = 5, size = 1, xDiff, yDiff){
  if (density > 1){density=1}
  xBins = gridOutput[[1]]
  yBins = gridOutput[[2]]
  freqMat = gridOutput[[3]]
  pointsToGrid = gridOutput[[4]]
  xStart = c()
  yStart = c()
  xEnd = c()
  yEnd = c()
  startingGrids = c(seq(nrow(freqMat)^2 - nrow(freqMat) + 1, 1, by=-1*nrow(freqMat)), seq(2, ncol(freqMat), by=1)) # which grids diagnol will start
  diagnolDraw = TRUE # wheater to draw in current diagnol or not
  slope = diff(yDiff)/diff(xDiff)
  adjustmentFactorX = convertSizeToCartesian(size, xDiff, 'x') * cos(atan(slope)) # trignometry ensures adjusted point remains on diagnol
  adjustmentFactorY = convertSizeToCartesian(size, yDiff, 'y') * sin(atan(slope))

  for (startingGrid in startingGrids){

    ## deals with determining grids in diagnol
    endingVal = nrow(freqMat)^2 # last grid of diagnol
    if (startingGrid <= ncol(freqMat) & startingGrid > 1){
      endingVal = nrow(freqMat)^2 - ((startingGrid - 1)*nrow(freqMat))
    }

    diagnol = seq(startingGrid, endingVal, by=nrow(freqMat) + 1) # sequence of grids in diagnol

    ## deals with wheather diagnol should be drawn or not according to density
    startingGridIndex = which(startingGrids == startingGrid)
    if (density < 1){
      diagnolDraw = (as.integer(startingGridIndex * density)*(1/density)) == startingGridIndex
    }

    lineDraw = FALSE
    prevGrid = 0

    for (grid in diagnol){

      # converts grid number to equivalent row and column
      row = as.integer(grid/(nrow(freqMat)))+1
      col = grid %% nrow(freqMat)
      if (col == 0){ col = ncol(freqMat)}
      if (row > nrow(freqMat)){ row = nrow(freqMat)}

      currentGridVal = freqMat[row, col] # number of points in current grid
      if (!lineDraw & prevGrid == 0 & currentGridVal != 0 & diagnolDraw){ # when to start a line
        xStart = c(xStart, xBins[col] - adjustmentFactorX)
        yStart = c(yStart, yBins[row] + adjustmentFactorY)
        lineDraw = TRUE
      }


      if (lineDraw & currentGridVal == 0 & diagnolDraw){ # when to end a line
        gridBefore = ifelse(is.na(diagnol[which(diagnol==grid)-1]), grid, diagnol[which(diagnol==grid)-1]) # getting the previous grid in diagnol
        row = as.integer(gridBefore/(nrow(freqMat)))+1
        col = gridBefore %% nrow(freqMat)
        xEnd = c(xEnd, xBins[col] + adjustmentFactorX)
        yEnd = c(yEnd, yBins[row] - adjustmentFactorY)
        lineDraw = FALSE
      }

      # dealing with sparse points
      surroundingCol = c((col-sparsity):(col-1), (col+1):(col+sparsity))
      surroundingCol = surroundingCol[surroundingCol > 0 & surroundingCol < nrow(freqMat)] # grids sparsity vertical distance away

      surroundingRow = c((row-sparsity):(row-1), (row+1):(row+sparsity))
      surroundingRow = surroundingRow[surroundingRow > 0 & surroundingRow< ncol(freqMat)] # grids sparsity vertical distance away


      if (sum(freqMat[row, surroundingCol]) + sum(freqMat[surroundingRow, col]) <= 2 & currentGridVal != 0 & !lineDraw){
        gridPoints = pointsToGrid[pointsToGrid$yIntervals == row & pointsToGrid$xIntervals == col, ]
        xStart = c(xStart, max(gridPoints$x) + adjustmentFactorX)
        xEnd = c(xEnd, min(gridPoints$x) - adjustmentFactorX)
        centerPointX = median(gridPoints$x)
        centerPointY = median(gridPoints$y)
        yStart = c(yStart, centerPointY - slope*(tail(xStart, 1) - centerPointX))
        yEnd = c(yEnd, centerPointY + slope*(centerPointX - tail(xEnd, 1)))
      }

      prevGrid = currentGridVal
    }
  }
  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}
