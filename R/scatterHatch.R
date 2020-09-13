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
#' @param patternList Aesthetics to be passed for each pattern (must be a list where each element has a named pattern)
#' @param colorPalette Colors to be used for each group.  Default is color-blind friendly.
#' @return ggplot2 object of scatterplot with hatched patterns for each group.
#' @export
scatterHatch <- function(data, x, y, factor, factorName, pointSize = 1.5, gridSize = NULL,
                         patternList = list(list(pattern="horizontal"), list(pattern="vertical"), list(pattern="positiveDiagonal"), list(pattern="negativeDiagonal")),
                         colorPalette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")){
  # getting pattern order sorted out
  patternOrder = c("horizontal","vertical","positiveDiagonal","negativeDiagonal")
  if (length(patternList) >= length(levels(factor))){ # when all patterns aesthetics changed
    patternOrder = sapply(patternList, function(x){x[["pattern"]]})
  }

  # getting legend ready
  legendDF = data.frame(x=numeric(), y=numeric(), ids=as.character())
  names = colnames(legendDF)
  legendIcons = list()

  # figuring out how many groups present
  groupNum = 1
  groups <- levels(factor)
  groups <- groups

  # creating the master plot
  plt = ggplot(data=data, aes(x=x, y=y))
  xDiff = ggplot_build(plt)$layout$panel_params[[1]]$x.range # gets x range
  yDiff = ggplot_build(plt)$layout$panel_params[[1]]$y.range # gets y range
  plt = plt + lims(x=xDiff, y=yDiff)

  # creating the patterns for each group
  for (group in groups){
    # setting default aesthetics
    lineType = "solid"
    lineColor = "black"
    lineAlpha = 1
    lineWidth = ifelse(pointSize < 0.5, 2*pointSize/.pt, pointSize/.pt)
    pointAlpha = 0.4
    gridSize = ifelse(is.null(gridSize), as.integer(500*exp(-pointSize/2.2)+43.44965), gridSize) # grid size follows a exponential decay function in terms of pointSize
    density = 1/4
    sparsity = as.integer(0.02*gridSize)

    xGroup = x[factor == group] # gets the points for each group
    yGroup = y[factor == group]
    groupData = data[factor == group, ]

    # getting the necessary aesthetics for each pattern
    pattern = patternOrder[groupNum]
    currentPatternAes = list(pattern=patternOrder[groupNum])
    for (i in 1:length(patternList)){
      if (length(patternList) == 0){ break}
      if (is.null(patternList[[i]]$pattern)){stop("Specify pattern in patternList argument!")}
      if (patternList[[i]]$pattern == pattern){
        currentPatternAes = patternList[[i]]  # aesthetics for given pattern
        patternList = patternList[-i] # to allow duplicate patterns with different aesthetics
        break
      }
    }

    if (!(is.null(currentPatternAes$density))){ density =currentPatternAes$density}
    if (!(is.null(currentPatternAes$sparsity))){ sparsity = currentPatternAes$sparsity}
    if (!(is.null(currentPatternAes$lineType))){ lineType = currentPatternAes$lineType}
    if (!(is.null(currentPatternAes$lineWidth))){ lineWidth = currentPatternAes$lineWidth}
    if (!(is.null(currentPatternAes$lineColor))){ lineColor = currentPatternAes$lineColor}
    if (!(is.null(currentPatternAes$pointAlpha))){ pointAlpha = currentPatternAes$pointAlpha}

    # plot points for each group
    plt = plt + geom_point(data=groupData, x=xGroup, y=yGroup, color=colorPalette[groupNum], alpha=pointAlpha, size=pointSize)

    # handles creating the legend icon
    legendDF = rbind(legendDF, c(median(xGroup), median(yGroup), group))
    legendIcons[[length(legendIcons) + 1]] = list(colorPalette[groupNum], lineColor, lineType, pattern, lineWidth, lineAlpha)
    colnames(legendDF) = names

    # get grid of each group
    groupGrid = countGridPoints(xGroup, yGroup, xDiff, yDiff, n=gridSize)
    if (pattern == "horizontal"){
      lineCoords = drawHorizontal(groupGrid, density=density, size=pointSize, sparsity=sparsity, xDiff, yDiff)
    }

    if (pattern == "vertical"){
      lineCoords = drawVertical(groupGrid, density=density, size=pointSize, sparsity=sparsity, xDiff, yDiff)
    }

    if (pattern == "positiveDiagonal"){
      lineCoords = drawPositiveDiagonal(groupGrid, density=density, size=pointSize, sparsity=sparsity, xDiff, yDiff)
    }

    if (pattern == "negativeDiagonal"){
      lineCoords = drawNegativeDiagonal(groupGrid, density=density, size=pointSize, sparsity=sparsity, xDiff, yDiff)
    }

    plt = plt + geom_segment(data=lineCoords, aes(x=xStart, y=yStart, xend=xEnd, yend=yEnd), alpha=lineAlpha, size=lineWidth, linetype=lineType, color=lineColor)

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
