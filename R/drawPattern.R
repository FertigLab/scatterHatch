drawPattern <- function(x, y, xDiff, yDiff, patternAes,
                        gridSize = NULL, sparseGroupPoints = NULL, pointSize = 1){


  # getting the pattern for the group
  if (length(patternAes) == 0){ stop("No given aesthetics!")}
  if (is.null(patternAes$pattern)){ stop("Specify pattern in patternList argument!")}
  pattern = patternAes$pattern

  # default grid size
  gridSize = ifelse(is.null(gridSize), as.integer(500*exp(-pointSize/2.2)+43.44965), gridSize) # grid size follows a exponential decay function in terms of pointSize


  # default density if not given
  if (is.null(patternAes$density)){
    density = 1/2
    if (pattern %in% c("horizontal", "-", "vertical", "|", "checkers", "+")){
      density = 1/4
    }
    if (pattern %in% c("positiveDiagonal", "/", "negativeDiagonal", "\\", "cross", "x")){
      density = 1/3
    }
  }


  # get grid of each group
  if (pattern %in% c("blank", "")){ # no line segments are then plotted for given group
    return(data.frame(x = c(), y = c(), xend = c(), yend = c()))
  }

  else if (pattern %in% c("horizontal","-")){
    groupGrid = countGridPoints(x, y, xDiff, yDiff, n=gridSize)
    lineCoords = drawHorizontal(groupGrid, density=density, pointSize=pointSize, xDiff, yDiff, xDiff, yDiff, sparseGroupPoints)

    adjustmentFactorX = convertSizeToCartesian(pointSize, xDiff, 'x')
    lineCoords$xStart = lineCoords$xStart - adjustmentFactorX
    lineCoords$xEnd = lineCoords$xEnd + adjustmentFactorX
  }

  else {
    for (a in angle){
      rotatedCoords = rotateCoords(x, y, angle = a) # rotating group coordinates
      rotatedCoordsRange = rotateCoords(c(xDiff[1], xDiff[1], xDiff[2], xDiff[2]), c(yDiff[1], yDiff[2], yDiff[1], yDiff[2]), a) # rotating coordinate bounds
      rotatedxDiff = range(rotatedCoordsRange$x)
      rotatedyDiff = range(rotatedCoordsRange$y)
      rotatedGroupGrid = countGridPoints(rotatedCoords$x, rotatedCoords$y, rotatedxDiff, rotatedyDiff, n=gridSize)

      lineCoords = drawHorizontal(rotatedGroupGrid, density=density, pointSize=pointSize, xDiff, yDiff, rotatedxDiff, rotatedyDiff, sparseGroupPoints)
      rotatedStartPoints = rotateCoords(lineCoords$xStart, lineCoords$yStart, -a)
      rotatedEndPoints = rotateCoords(lineCoords$xEnd, lineCoords$yEnd, -a)

      adjustmentFactorX = convertSizeToCartesian(pointSize, xDiff, 'x') * cos(-a/180 * pi)
      adjustmentFactorY = convertSizeToCartesian(pointSize, yDiff, 'y') * sin(-a/180 * pi)

      lineCoords$xStart = rotatedStartPoints$x - adjustmentFactorX
      lineCoords$yStart = rotatedStartPoints$y - adjustmentFactorY
      lineCoords$xEnd = rotatedEndPoints$x + adjustmentFactorX
      lineCoords$yEnd = rotatedEndPoints$y + adjustmentFactorY

    }
  }
  names(lineCoords) = c('x', 'y', 'xend', 'yend')

  return(lineCoords)

}
