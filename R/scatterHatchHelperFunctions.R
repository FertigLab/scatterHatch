#' Creates the default patternList scatterHatch will use
#' @param patternList Aesthetics to be passed for each pattern
#' @param nGroups Number of groups
#' @param pointSize Point size for the scatterplot
#' @param pointAlpha Transparency of points
#' @noRd

defaultPatternList <- function(patternList, nGroups){
  
    ## default aesthetics
    patterns <- c("-","|","/","\\", "x", "+", "")
  
    ## creating pattern list if none given
    if (is.null(patternList)){ 
        patterns <- rep(patterns, ceiling(nGroups/length(patterns)))
    
        ## initializing patternList
        patternList <- vector(mode="list", length=nGroups)
        patternList <- lapply(seq(nGroups), function(i){
                           patternList[[i]] = list(pattern=patterns[i])})
    }
  
    ## checks if patternList length is ok
    if (length(patternList) < nGroups){
        stop("The length of patternList must be greater than or equal to the number of groups present.")
    }
  
    return(patternList)
  
}

#' Creates the default colorPalette scatterHatch will use
#' @param colorPalette Color Palette
#' @param patternList Aesthetics of each pattern
#' @param nGroups Number of groups
#' @noRd

defaultColorPalette <- function(colorPalette, patternList, nGroups){
    if (is.null(colorPalette)){
        ## Color-Blind friendly colors from dittoSeq package
        dittoColors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                         "#D55E00", "#CC79A7", "#666666", "#AD7700", "#1C91D4",
                         "#007756", "#D5C711", "#005685", "#A04700", "#B14380", 
                         "#4D4D4D", "#FFBE2D", "#80C7EF", "#00F6B3", "#F4EB71",
                         "#06A5FF", "#FF8320", "#D99BBD", "#8C8C8C", "#FFCB57", 
                         "#9AD2F2", "#2CFFC6", "#F6EF8E", "#38B7FF", "#FF9B4D",
                         "#E0AFCA", "#A3A3A3", "#8A5F00", "#1674A9", "#005F45", 
                         "#AA9F0D", "#00446B", "#803800", "#8D3666", "#3D3D3D")
         colorPalette <- rep(dittoColors, times=ceiling(nGroups/40))[seq(nGroups)]
    }
  
    ## finds number of unique patterns
    nOfPatterns <- length(unique(vapply(patternList, function(i){i[[1]]}, character(1)))) 
  
    ## if not enough color/pattern combinations
    if (length(unique(colorPalette)) * nOfPatterns < nGroups){
        stop("Not enough unique combinations of patterns and columns for each group.")
    }
  
    if (length(unique(colorPalette)) < nGroups){
        warning("Same point colors will be repeated with different hatching patterns!")
    }
  
    return(colorPalette)
}

#' Creates the base scatter plot for scatterHatch (without patterns)
#' @param data Dataframe
#' @param x x-coordinates of points
#' @param y y-coordinates of points
#' @param factor factor variable of points
#' @param colorPalette Colors of each group
#' @param pointSize ggplot point size
#' @param pointAlpha Transparency of points in the scatterplot
#' @noRd
basePlot <- function(data, x, y, factor, colorPalette, 
                          pointSize, pointAlpha){
  ## creating the master plot
  plt <- ggplot2::ggplot(data=data, ggplot2::aes(x=x, y=y))
  xRange <- ggplot2::ggplot_build(plt)$layout$panel_params[[1]]$x.range
  yRange <- ggplot2::ggplot_build(plt)$layout$panel_params[[1]]$y.range
  plt <- plt + ggplot2::lims(x=xRange, y=yRange)
  
  ## plot points for all the groups
  plt <- plt + ggplot2::geom_point(ggplot2::aes(color=factor), 
                                  alpha=pointAlpha, size=pointSize, 
                                  stroke=0, show.legend=FALSE) +
    ggplot2::scale_color_manual(values=colorPalette)
  
  return(list(plt, xRange, yRange))
}

#' Adds default aesthetics in a pattern if missing
#' @param patternAes List specifying pattern aesthetics
#' @param pointSize ggplot point size
#' @param pointAlpha Transparency of each point
#' @noRd
addPatternAesDefaults <- function(patternAes, pointSize, pointAlpha){
  if (length(patternAes) == 0){ stop("No given aesthetics!")}
  if (is.null(patternAes$pattern)){ 
    stop("Specify pattern in patternList argument!")
  }
  
  pattern <- patternAes$pattern
  
  ## ensures pattern line rotation is what's expected by user
  if (!is.null(patternAes$angle)){ 
    patternAes$angle <- -patternAes$angle
  }
  
  ## what angle each pattern translates to
  if (is.null(patternAes$angle)){
    if (pattern %in% c("horizontal", "-", "vertical", "|")){
      if (pattern %in% c("horizontal", "-")){ patternAes$angle <- 0}
      if (pattern %in% c("vertical", "|")){ patternAes$angle <- -90}
    }else{
      if (pattern %in% c("positiveDiagonal", "/")){ patternAes$angle <- -135}
      if (pattern %in% c("negativeDiagonal", "\\")){ patternAes$angle <- -45}
      if (pattern %in% c("cross", "x")){ patternAes$angle <- c(-45, -135)}
      if (pattern %in% c("checkers", "+")){ patternAes$angle <- c(0, -90)}
    }
  }
  
  ## default density if not given
  if (is.null(patternAes$density)){
    patternAes$density <- 1/3
    if (pattern %in% c("horizontal", "-", "vertical", "|", "checkers", "+")){
      patternAes$density <- 1/2
    }
    if (pattern %in% 
        c("positiveDiagonal", "/", "negativeDiagonal", "\\", "cross", "x")){
      patternAes$density <- 1/2
    }
  }
  
  ## default aesthetics
  if (is.null(patternAes$lineType)){ patternAes$lineType <- "solid"}
  if (is.null(patternAes$lineColor)){ patternAes$lineColor <- "black"}
  if (is.null(patternAes$lineAlpha)){ patternAes$lineAlpha <- 1}
  if (is.null(patternAes$lineWidth)){ 
    patternAes$lineWidth <- ifelse(pointSize < 0.5, 2*pointSize/ggplot2::.pt, 
                                   pointSize/ggplot2::.pt)
  }
  patternAes$pointAlpha = pointAlpha
  return(patternAes)
}

#' Adds pattern legend icon information
#' @param legendIcons Aesthetics of each pattern to build legend icon
#' @param patternAes List specifying pattern aesthetics
#' @param pointColor Color of points in group
#' @noRd
addLegendIconInfo <- function(legendIcons, patternAes, pointColor){
  legendIcons[[length(legendIcons) + 1]] <- 
    list(pointColor, patternAes$lineColor, patternAes$lineType,
         patternAes$pattern, patternAes$lineWidth, 
         patternAes$lineAlpha, patternAes$angle)
  return(legendIcons)
}

#' Adds in line segments for a pattern
#' @param plot ggplot plot object scatterHatch is building
#' @param xGroup x-coordinates of the group
#' @param yGroup y-coordinates of the group
#' @param xRange x-range of plot
#' @param yRange y-range of plot
#' @param nBins Number of bins in grid
#' @param patternAes Aesthetics of a pattern
#' @param pointSize Size of points in scatterHatch
#' @param sparsePoints Logical Vector of sparse points in group
#' @noRd
addSegments <- function(plot, xGroup, yGroup, xRange, yRange, nBins, 
                        patternAes, pointSize, sparsePoints){
  xEnd <- xStart <- yEnd <- yStart <- NULL
  for (a in patternAes$angle){
    ## rotating group coordinates
    rotatedCoords <- rotateCoords(xGroup, yGroup, angle=a) 
    rotatedCoordsRange <- rotateCoords(c(xRange[1], xRange[1], xRange[2], 
                                         xRange[2]), c(yRange[1], yRange[2],
                                                       yRange[1], yRange[2]), a)
    rotatedxRange <- range(rotatedCoordsRange$x)
    rotatedyRange <- range(rotatedCoordsRange$y)
    rotatedgridOutput <- countGridPoints(rotatedCoords$x, rotatedCoords$y, 
                                         rotatedxRange, rotatedyRange, n=nBins)
    ## getting individual line segments
    groupLineCoords <- drawHorizontal(rotatedgridOutput, patternAes$density, 
                                      pointSize, xRange, yRange, rotatedxRange, 
                                      rotatedyRange, sparsePoints)
    ## adjusting lines based on sizes of a point
    adjX <- convertSizeToCartesian(pointSize, xRange, 'x')
    adjY <- convertSizeToCartesian(pointSize, yRange, 'y')
    
    ## rotation matrix
    R <- matrix(c(cos(a/180 * pi),sin(a/180 * pi),
                  -sin(a/180 * pi),cos(a/180 * pi)), 2, 2) 
    ## rotating adjustment based on angle
    rotatedAdjX <- suppressWarnings(diag(sqrt(
      R %*% diag(c(adjX, adjY), 2, 2)^2 %*% t(R)))[1])
    if (a == 0){ rotatedAdjX <- adjX}
    ## converting back to regular coordinates
    rotatedStartPoints <- rotateCoords(groupLineCoords$xStart - rotatedAdjX, 
                                       groupLineCoords$yStart, -a)
    rotatedEndPoints <- rotateCoords(groupLineCoords$xEnd + rotatedAdjX, 
                                     groupLineCoords$yEnd, -a)
    ## adding line segments to plot
    groupLineCoords$xStart <- rotatedStartPoints$x
    groupLineCoords$yStart <- rotatedStartPoints$y
    groupLineCoords$xEnd <- rotatedEndPoints$x
    groupLineCoords$yEnd <- rotatedEndPoints$y
    plot <- plot + ggplot2::geom_segment(data=groupLineCoords, 
                                        ggplot2::aes(x=xStart, y=yStart, 
                                                    xend=xEnd, yend=yEnd), 
                                        alpha=patternAes$lineAlpha, 
                                        size=patternAes$lineWidth, 
                                        linetype=patternAes$lineType, 
                                        color=patternAes$lineColor)
  }
  return(plot)
}

#' Adds legend for scatterHatch
#' @param plot ggplot plot object scatterHatch is building
#' @param legendDF Dataframe to build legend
#' @param legendIcons Aesthetics to build legend icons for each group
#' @param factor Factor variable
#' @param legendTitle Title of legend
#' @noRd
addLegend <- function(plot, legendDF, legendIcons, factor, legendTitle){
  ids <- x <- y <- NULL
  legendDF$legendIcons <- legendIcons

  ## Adding in the factor names and pattern info to legend
  scale_image <- function(..., guide="legend"){
    ggplot2::scale_discrete_manual(aes="ids",
                                   labels=as.character(levels(factor)),
                                   values=legendDF$legendIcons)
  }

  ## renders custom legend
  plot <- plot + geom_imagePoint(data=legendDF, ggplot2::aes(
    x=as.numeric(x), y=as.numeric(y), 
    ids=as.character(ids))) + scale_image()

  ## renaming legend title
  plot$labels$ids <- legendTitle
  return(plot)
}