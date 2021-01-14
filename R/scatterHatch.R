# Tejas Guha - September 13, 2020

#' Creates a scatterplot with hatched patterns.
#'
#' This function creates a scatterplot with hatched patterns by using geom_segment().
#' Also creates the legend to represent each hatched group.  The aesthetics defining each pattern,
#' like the lineType and lineColor are all passed to geom_segment().
#'
#' @param data Dataset (what normally would be passed to ggplot2)
#' @param x x-coordinates of the plot
#' @param y y-coordinates of the plot
#' @param factor Factor vector that defines what group each point is part of
#' @param factorName Title of the legend
#' @param pointSize Point size for the scatterplot
#' @param gridSize Controls the precision of the hatched pattern.  Larger values correspond to greater precision.  Default follows a exponential decay function based on point size.
#' @param sparsePoints Logical Vector denoting points annotated as sparse.  If NULL, default sparsity detector will be used to annotate sparse points in dataset.
#' @param patternList Aesthetics to be passed for each pattern (must be a list where each element has a named pattern)
#' @param colorPalette Colors to be used for each group.  Default is color-blind friendly.
#' @return ggplot2 object of scatterplot with hatched patterns for each group.
#' @export
scatterHatch <- function(data, x, y, factor, factorName, pointSize = 1.5, gridSize = NULL, sparsePoints = NULL,
                         patternList = NULL, colorPalette = NULL){

  # figuring out how many groups present
  groups <- unique(factor)

  # default aesthetics
  lineType = "solid"
  lineColor = "black"
  lineAlpha = 1
  lineWidth = ifelse(pointSize < 0.5, 2*pointSize/.pt, pointSize/.pt)
  pointAlpha = 0.4
  gridSize = ifelse(is.null(gridSize), as.integer(500*exp(-pointSize/2.2)+43.44965), gridSize) # grid size follows a exponential decay function in terms of pointSize
  patterns = c("-","|","/","\\", "x", "+", "")

  if (is.null(patternList)){ # creating pattern list if none given
    patterns = rep(patterns, ceiling(length(groups)/length(patterns)))
    patternList = vector(mode = "list", length = length(groups)) # initializing patternList
    patternList = lapply(1:length(groups), function(i){patternList[[i]] =
      list(pattern = patterns[i], lineType = lineType, lineAlpha = lineAlpha, lineWidth = lineWidth, pointAlpha = pointAlpha)})
  }

  if (is.null(colorPalette)){
    colorPalette = dittoSeq::dittoColors(reps = ceiling(length(groups)/40))[1:length(groups)]
  }

  # getting legend ready
  legendDF = data.frame(x=numeric(), y=numeric(), ids=as.character())
  names = colnames(legendDF)
  legendIcons = list()

  # creating the master plot
  plt = ggplot(data=data, aes(x=x, y=y))
  xDiff = ggplot_build(plt)$layout$panel_params[[1]]$x.range # gets x range
  yDiff = ggplot_build(plt)$layout$panel_params[[1]]$y.range # gets y range
  plt = plt + lims(x=xDiff, y=yDiff)

  groupNum = 1

  # creating the patterns for each group
  for (group in groups){
    xGroup = x[factor == group] # gets the points for each group
    yGroup = y[factor == group]
    groupData = data[factor == group, ]

    # finding the sparse points for the group if any are given
    if (!is.null(sparsePoints)){ sparseGroupPoints = sparsePoints[factor == group]}
    if (is.null(sparsePoints)){ sparseGroupPoints = NULL}

    currentPatternAes = patternList[[groupNum]]  # aesthetics for given pattern

    if (length(currentPatternAes) == 0){ stop("No given aesthetics!")}
    if (is.null(currentPatternAes$pattern)){ stop("Specify pattern in patternList argument!")}
    angle = NULL

    pattern = currentPatternAes$pattern

    # what angle each pattern translates to
    if (is.null(currentPatternAes$angle)){
      if (pattern %in% c("horizontal", "-", "vertical", "|")){
        if (pattern %in% c("horizontal", "-")){ angle = 0}
        if (pattern %in% c("vertical", "|")){ angle = -90}
      }else{
        if (pattern %in% c("positiveDiagonal", "/")){ angle = -135}
        if (pattern %in% c("negativeDiagonal", "\\")){ angle = -45}
        if (pattern == "x"){ angle = c(-45, -135)}
        if (pattern %in% c("checkers", "+")){ angle = c(0, -90)}
      }
    }

    # ensures pattern line rotation is what's expected by user
    if (!is.null(currentPatternAes$angle)){ angle = currentPatternAes$angle; angle = -angle}

    # default density if not given
    if (is.null(currentPatternAes$density)){
      density = 1/2
      if (pattern %in% c("horizontal", "-", "vertical", "|", "checkers", "+")){
        density = 1/4
      }
      if (pattern %in% c("positiveDiagonal", "/", "negativeDiagonal", "\\", "x")){
        density = 1/3
      }
    }


    # if any missing aesthetics for a pattern use default
    if (is.null(currentPatternAes$lineType)){ lineType = lineType}
    if (is.null(currentPatternAes$lineWidth)){ lineWidth = lineWidth}
    if (is.null(currentPatternAes$lineColor)){ lineColor = lineColor}
    if (is.null(currentPatternAes$pointAlpha)){ pointAlpha = pointAlpha}


    # plot points for each group
    plt = plt + geom_point(data=groupData, x=xGroup, y=yGroup, color=colorPalette[groupNum], alpha=pointAlpha, size=pointSize)

    # handles creating the legend icon
    legendDF = rbind(legendDF, c(median(xGroup), median(yGroup), group))
    legendIcons[[length(legendIcons) + 1]] = list(colorPalette[groupNum], lineColor, lineType, pattern, lineWidth, lineAlpha, angle)
    colnames(legendDF) = names

    # get grid of each group
    if (pattern %in% c("blank", "")){ # no line segments are then plotted for given group
    }

    else if (pattern %in% c("horizontal","-")){
      groupGrid = countGridPoints(xGroup, yGroup, xDiff, yDiff, n=gridSize)
      lineCoords = drawHorizontal(groupGrid, density=density, pointSize=pointSize, xDiff, yDiff, xDiff, yDiff, sparseGroupPoints)

      plt = plt + geom_segment(data=lineCoords, aes(x=xStart, y=yStart, xend=xEnd, yend=yEnd), alpha=lineAlpha, size=lineWidth, linetype=lineType, color=lineColor)
    }

    else {
      for (a in angle){
        rotatedCoords = rotateCoords(xGroup, yGroup, angle = a) # rotating group coordinates
        rotatedCoordsRange = rotateCoords(c(xDiff[1], xDiff[1], xDiff[2], xDiff[2]), c(yDiff[1], yDiff[2], yDiff[1], yDiff[2]), a) # rotating coordinate bounds
        rotatedxDiff = range(rotatedCoordsRange$x)
        rotatedyDiff = range(rotatedCoordsRange$y)
        rotatedGroupGrid = countGridPoints(rotatedCoords$x, rotatedCoords$y, rotatedxDiff, rotatedyDiff, n=gridSize)

        lineCoords = drawHorizontal(rotatedGroupGrid, density=density, pointSize=pointSize, xDiff, yDiff, rotatedxDiff, rotatedyDiff, sparseGroupPoints)
        rotatedStartPoints = rotateCoords(lineCoords$xStart, lineCoords$yStart, -a)
        rotatedEndPoints = rotateCoords(lineCoords$xEnd, lineCoords$yEnd, -a)

        lineCoords$xStart = rotatedStartPoints$x
        lineCoords$yStart = rotatedStartPoints$y
        lineCoords$xEnd = rotatedEndPoints$x
        lineCoords$yEnd = rotatedEndPoints$y

        plt = plt + geom_segment(data=lineCoords, aes(x=xStart, y=yStart, xend=xEnd, yend=yEnd), alpha=lineAlpha, size=lineWidth, linetype=lineType, color=lineColor)
      }

    }

    groupNum = groupNum + 1
  }

  # creating the legend
  legendDF$legendIcons = legendIcons
  scale_image <- function(..., guide="legend"){ # adding in the factor names and pattern info to legend
    scale_discrete_manual(aes="ids", labels=as.character(levels(factor)), values=legendDF$legendIcons)
  }
  plt = plt + geom_imagePoint(data=legendDF, aes(x = as.numeric(x), y = as.numeric(y), ids = as.character(ids))) + scale_image()
  plt$labels$ids <- factorName # renaming legend title
  return(plt)
}
