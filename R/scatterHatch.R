# Tejas Guha - September 13, 2020

#' Creates a scatterplot with hatched patterns.
#'
#' This function creates a scatterplot with hatched patterns 
#' by using geom_segment().  Also creates the legend to represent each 
#' hatched group.  The aesthetics defining each pattern,
#' like the lineType and lineColor are all passed to geom_segment().
#'
#' @param data Dataset to be used
#' @param x Column name of x-coordinates
#' @param y Column name of y-coordinates
#' @param factor Column name of factor that defines groupings
#' @param legendTitle Title of the legend
#' @param pointSize Point size for the scatterplot
#' @param pointAlpha Transparency of points in the scatterplot
#' @param nBins Controls the precision of the hatched pattern.  
#' Larger values correspond to greater precision.  
#' Default follows a exponential decay function based on point size.
#' @param sparsePoints Logical Vector denoting points annotated as sparse.  
#' If NULL, default sparsity detector will be used to annotate sparse points.
#' @param patternList Aesthetics to be passed for each pattern 
#' (must be a list where each element has a named pattern)
#' @param colorPalette Colors to be used for each group.  
#' Default is color-blind friendly.
#' @return ggplot2 object of scatterplot with hatched patterns for each group.
#' @export
#' @importFrom grDevices dev.size
#' @importFrom stats median
#' @examples
#' scatterHatch(pdacData, "Xt", "Yt", "frame")

scatterHatch <- function(data, x, y, factor, legendTitle = "", pointSize = 1, 
                         pointAlpha = 0.5, gridSize = NULL, sparsePoints = NULL, 
                         patternList = NULL, colorPalette = NULL){
    if (!(x %in% names(data))){ stop("x column name not present in dataset.")}
    if (!(y %in% names(data))){ stop("y column name not present in dataset.")}
    if (!(factor %in% names(data))){ stop("factor column name not present in dataset.")}
    x <- data[, x]; y <- data[, y]
    if (!is.numeric(x)){ stop("x column is not numeric.")}
    if (!is.numeric(y)){ stop("y column is not numeric.")}
    factor <- as.factor(data[, factor])
    nGroups <- length(levels(factor))
  
    ## grid size follows a exponential decay function in terms of pointSize
    gridSize <- ifelse(is.null(gridSize), 100*pointSize, gridSize)
    patternList <- defaultPatternList(patternList, nGroups)
    colorPalette <- defaultColorPalette(colorPalette, patternList, nGroups)
    ## getting legend ready
    legendDF <- data.frame(x=numeric(), y=numeric(), ids=as.character())
    names <- colnames(legendDF)
    legendIcons <- list()
    ## building the base plot with points
    output <- basePlot(data, x, y, factor, colorPalette, pointSize, pointAlpha)
    plt <- output[[1]]; xRange <- output[[2]]; yRange <- output[[3]]
    groupNum <- 1

    ## creating the patterns for each group
    for (group in levels(factor)){
        ## gets the points for each group
        xGroup <- x[factor == group]; yGroup <- y[factor == group]
        ## finding the sparse points for the group if any are given
        sparseGroupPoints <- sparsePoints[factor == group]
        ## aesthetics for given pattern
        currentPatternAes <- addPatternAesDefaults(patternList[[groupNum]], pointSize, pointAlpha)
    
        ## handles creating the legend icon
        legendDF <- rbind(legendDF, c(median(xGroup), median(yGroup), group))
        colnames(legendDF) <- names
        legendIcons <- addLegendIconInfo(legendIcons, currentPatternAes, colorPalette[groupNum])
    
        ## adding in line segments for group
        if (!(currentPatternAes$pattern %in% c("blank", "")))
        { # no line segments
          plt <- addSegments(plt, xGroup, yGroup, xRange, yRange, gridSize, 
                            currentPatternAes, pointSize, sparseGroupPoints)}
          groupNum = groupNum + 1
    }
    ## creating the legend
    plt <- addLegend(plt, legendDF, legendIcons, factor, legendTitle)
    return(plt + ggplot2::theme_classic()) # adding in classic theme
}