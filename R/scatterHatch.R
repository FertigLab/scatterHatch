# Tejas Guha - September 13, 2020

#' Creates a scatterplot with hatched patterns.
#'
#' This function creates a scatterplot with hatched patterns by using geom_segment().
#' Also creates the legend to represent each hatched group.  The aesthetics defining each pattern,
#' like the lineType and lineColor are all passed to geom_segment().
#'
#' @param data Dataset to be used (Dataframe)
#' @param x Column name of x-coordinates (String)
#' @param y Column name of y-coordinates (String)
#' @param factor Column name of factor that defines groupings of each point (String)
#' @param legendTitle Title of the legend
#' @param pointSize Point size for the scatterplot
#' @param pointAlpha Transparency of points in the scatterplot
#' @param gridSize Controls the precision of the hatched pattern.  Larger values correspond to greater precision.  Default follows a exponential decay function based on point size.
#' @param sparsePoints Logical Vector denoting points annotated as sparse.  If NULL, default sparsity detector will be used to annotate sparse points in dataset.
#' @param patternList Aesthetics to be passed for each pattern (must be a list where each element has a named pattern)
#' @param colorPalette Colors to be used for each group.  Default is color-blind friendly.
#' @return ggplot2 object of scatterplot with hatched patterns for each group.
#' @export

scatterHatch <- function(data, x, y, factor, legendTitle = "", pointSize = 1, pointAlpha = 0.5, gridSize = NULL, sparsePoints = NULL,
                         patternList = NULL, colorPalette = NULL){

  if (!(x %in% names(data))){ stop("x column name not present in dataset.")}
  if (!(y %in% names(data))){ stop("y column name not present in dataset.")}
  if (!(factor %in% names(data))){ stop("factor column name not present in dataset.")}
  x = data[, x]
  y = data[, y]

  if (!is.numeric(x)){ stop("x column is not numeric.")}
  if (!is.numeric(y)){ stop("y column is not numeric.")}

  factor = as.factor(data[, factor])

  # figuring out how many groups present
  groups = levels(factor)

  # default aesthetics
  lineType = "solid"
  lineColor = "black"
  lineAlpha = 1
  lineWidth = ifelse(pointSize < 0.5, 2*pointSize/ggplot2::.pt, pointSize/ggplot2::.pt)
  gridSize = ifelse(is.null(gridSize), as.integer(500*exp(-pointSize/2.2)+43.44965), gridSize) # grid size follows a exponential decay function in terms of pointSize
  patterns = c("-","|","/","\\", "x", "+", "")

  if (is.null(patternList)){ # creating pattern list if none given
    patterns = rep(patterns, ceiling(length(groups)/length(patterns)))
    patternList = vector(mode = "list", length = length(groups)) # initializing patternList
    patternList = lapply(1:length(groups), function(i){patternList[[i]] =
      list(pattern = patterns[i], lineType = lineType, lineAlpha = lineAlpha, lineWidth = lineWidth, pointAlpha = pointAlpha)})
  }

  if (length(patternList) != length(groups)){ # checks if patternList length is ok
    stop("The length of patternList must be equal to the number of groups present.")
  }

  if (is.null(colorPalette)){
    colorPalette = dittoSeq::dittoColors(reps = ceiling(length(groups)/40))[1:length(groups)]
  }

  if (length(unique(colorPalette)) < length(groups)){
    warning("Same point colors will be repeated with different hatching patterns!")
  }

  # getting legend ready
  legendDF = data.frame(x=numeric(), y=numeric(), ids=as.character())
  names = colnames(legendDF)
  legendIcons = list()

  # creating the master plot
  plt = ggplot2::ggplot(data=data, ggplot2::aes(x=x, y=y))
  xDiff = ggplot2::ggplot_build(plt)$layout$panel_params[[1]]$x.range # gets x range
  yDiff = ggplot2::ggplot_build(plt)$layout$panel_params[[1]]$y.range # gets y range
  plt = plt + ggplot2::lims(x=xDiff, y=yDiff)

  # plot points for all the groups
  plt = plt + ggplot2::geom_point(ggplot2::aes(color = factor), alpha=pointAlpha, size=pointSize, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = colorPalette)

  lineCoords = data.frame(matrix(ncol = 4, nrow = 0))
  names(lineCoords) = c("xStart", "yStart", "xEnd", "yEnd")


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
        if (pattern %in% c("cross", "x")){ angle = c(-45, -135)}
        if (pattern %in% c("checkers", "+")){ angle = c(0, -90)}
      }
    }


    # ensures pattern line rotation is what's expected by user
    if (!is.null(currentPatternAes$angle)){ angle = currentPatternAes$angle; angle = -angle}

    # default density if not given
    if (is.null(currentPatternAes$density)){
      density = 1/2
      if (pattern %in% c("horizontal", "-", "vertical", "|", "checkers", "+")){
        density = 1/2
      }
      if (pattern %in% c("positiveDiagonal", "/", "negativeDiagonal", "\\", "cross", "x")){
        density = 1/2
      }
    }

    if (!is.null(currentPatternAes$density)){ density = currentPatternAes$density}

    # handles creating the legend icon
    legendDF = rbind(legendDF, c(median(xGroup), median(yGroup), group))
    legendIcons[[length(legendIcons) + 1]] = list(colorPalette[groupNum], lineColor, lineType, pattern, lineWidth, lineAlpha, angle)
    colnames(legendDF) = names

    # get grid of each group
    if (pattern %in% c("blank", "")){ # no line segments are then plotted for given group
    }

    else if (pattern %in% c("horizontal","-")){
      groupGrid = countGridPoints(xGroup, yGroup, xDiff, yDiff, n=gridSize)
      groupLineCoords = drawHorizontal(groupGrid, density=density, pointSize=pointSize, xDiff, yDiff, xDiff, yDiff, sparseGroupPoints)

      adjustmentFactorX = convertSizeToCartesian(pointSize, xDiff, 'x')
      #adjustmentFactorX = pointSize*ggplot2::.pt + ggplot2::.stroke*0.5/2;
      groupLineCoords$xStart = groupLineCoords$xStart - adjustmentFactorX
      groupLineCoords$xEnd = groupLineCoords$xEnd + adjustmentFactorX

      lineCoords = rbind(lineCoords, groupLineCoords)
    }

    else {
      for (a in angle){
        rotatedCoords = rotateCoords(xGroup, yGroup, angle = a) # rotating group coordinates
        rotatedCoordsRange = rotateCoords(c(xDiff[1], xDiff[1], xDiff[2], xDiff[2]), c(yDiff[1], yDiff[2], yDiff[1], yDiff[2]), a) # rotating coordinate bounds
        rotatedxDiff = range(rotatedCoordsRange$x)
        rotatedyDiff = range(rotatedCoordsRange$y)
        rotatedGroupGrid = countGridPoints(rotatedCoords$x, rotatedCoords$y, rotatedxDiff, rotatedyDiff, n=gridSize)

        groupLineCoords = drawHorizontal(rotatedGroupGrid, density=density, pointSize=pointSize, xDiff, yDiff, rotatedxDiff, rotatedyDiff, sparseGroupPoints)

        adjX = convertSizeToCartesian(pointSize, xDiff, 'x')
        adjY = convertSizeToCartesian(pointSize, yDiff, 'y')
        R = matrix(c(cos(a/180 * pi),sin(a/180 * pi),-sin(a/180 * pi),cos(a/180 * pi)),2,2) # rotation matrix
        rotatedAdjX = suppressWarnings(diag(sqrt(R %*% diag(c(adjX,adjY),2,2)^2 %*% t(R)))[1]) # handles rotating adjument based on rotation

        rotatedStartPoints = rotateCoords(groupLineCoords$xStart - rotatedAdjX, groupLineCoords$yStart, -a) # converting back to regular coordinates
        rotatedEndPoints = rotateCoords(groupLineCoords$xEnd + rotatedAdjX, groupLineCoords$yEnd, -a)

        groupLineCoords$xStart = rotatedStartPoints$x
        groupLineCoords$yStart = rotatedStartPoints$y
        groupLineCoords$xEnd = rotatedEndPoints$x
        groupLineCoords$yEnd = rotatedEndPoints$y

        lineCoords = rbind(lineCoords, groupLineCoords)
      }

    }

    groupNum = groupNum + 1
  }

  plt = plt + ggplot2::geom_segment(data=lineCoords, ggplot2::aes(x=xStart, y=yStart, xend=xEnd, yend=yEnd), alpha=lineAlpha, size=lineWidth, linetype=lineType, color=lineColor)

  # creating the legend
  legendDF$legendIcons = legendIcons
  scale_image <- function(..., guide="legend"){ # adding in the factor names and pattern info to legend
    ggplot2::scale_discrete_manual(aes="ids", labels=as.character(levels(factor)), values=legendDF$legendIcons)
  }
  plt = plt + geom_imagePoint(data=legendDF, ggplot2::aes(x = as.numeric(x), y = as.numeric(y), ids = as.character(ids))) + scale_image()
  plt$labels$ids <- legendTitle # renaming legend title

  plt = plt + ggplot2::theme_classic() # adding in classic theme
  return(plt)
}
