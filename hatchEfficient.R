library(ggplot2)
library(grid)
library(magick)
library(plyr)

#creating the sample dataframe to work with
pdacData = read.csv("D://umd//summer 2020//spatial-datasets-master//spatial-datasets-master//data//2018_CyCIF_PDAC//rawdata_Figure78_PDAC//rawdata_Figure7&8_PDAC.csv")
pdacData$cellID = paste0('cell_', 1:nrow(pdacData))
pdacData$Yt <- -pdacData$Yt
pancreas_frames = c(1:6, 27:31, 15:19, 40:44)
PDAC_frames = c(23:26, 35:37, 51:52, 64:65, 77)
small_intestines_frames = c(49:50, 63, 75:76, 88:89, 100:103, 112:116, 125:129, 137:140)
annotateLocation <- function(frame){
  if (frame %in% pancreas_frames){return("Pancreas")}
  if (frame %in% PDAC_frames){return("PDAC")}
  if (frame %in% small_intestines_frames){return("Small Intestine")}
  return("Other")
}
pdacData$location = sapply(pdacData$frame, annotateLocation)

sampleDF = pdacData[sample(nrow(pdacData), 10000),] # random sample of 1000 points from PDAC dataset
sampleDF = pdacData

# preparing helper functions to create the legend for graph

legendIcon <- function(color, lineColor="black", lineType = "solid", patternType){ 
  # tried to render the legend icons by creating new magick object but issue with grDevices prevented usage of ggsave
  # instead using grid to render legend icons
  if (patternType == "horizontal"){
    return(grobTree(
      circleGrob(0.5, 0.5, 0.5, gp=gpar(col=color, lwd=1)),
      linesGrob(x=c(0,1), y=c(0.5, 0.5)),
      linesGrob(x=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), y=c(0.25, 0.25)),
      linesGrob(x=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), y=c(0.75, 0.75)),
      gp = gpar(
        col = lineColor,
        fill = color,
        lwd = 1.5,
        lty = lineType
      )
    ))
  }
  if (patternType == "vertical"){
    return(grobTree(
      circleGrob(0.5, 0.5, 0.5, gp=gpar(col=color, lwd=1)),
      linesGrob(y=c(0,1), x=c(0.5, 0.5)),
      linesGrob(y=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), x=c(0.25, 0.25)),
      linesGrob(y=c(-sqrt((0.5^2)-(0.25^2)) + 0.5, sqrt((0.5^2)-(0.25^2)) +0.5), x=c(0.75, 0.75)),
      gp = gpar(
        col = lineColor,
        fill = color,
        lwd = 1.5,
        lty = lineType
      )
    ))
  }
  if (patternType == "positiveDiagnol"){
    return(grobTree(
      circleGrob(0.5, 0.5, 0.5, gp=gpar(col=color, lwd=1)),
      linesGrob(x=c(-sqrt((0.5)^2/2)+0.5,sqrt((0.5)^2/2)+0.5), y=c(-sqrt((0.5)^2/2)+0.5, sqrt((0.5)^2/2)+0.5)),
      linesGrob(x=c(0.01779486, 0.6322051), y=c(0.3677949, 0.9822051)),
      linesGrob(x=c(0.3677949, 0.9822051), y=c(0.01779486, 0.6322051)),
      gp = gpar(
        col = lineColor,
        fill = color,
        lwd = 1.75,
        lty = lineType
      )
    ))
  }
  if (patternType == "negativeDiagnol"){
    return(grobTree(
      circleGrob(0.5, 0.5, 0.5, gp=gpar(col=color, lwd=1)),
      linesGrob(x=c(-sqrt((0.5)^2/2)+0.5,sqrt((0.5)^2/2)+0.5), y=c(sqrt((0.5)^2/2)+0.5, -sqrt((0.5)^2/2)+0.5)),
      linesGrob(x=c(0.01779486, 0.6322051), y=c(0.6322051, 0.01779486)),
      linesGrob(x=c(0.3677949, 0.9822051), y=c(0.9822051, 0.3677949)),
      gp = gpar(
        col = lineColor,
        fill = color,
        lwd = 1.75,
        lty = lineType
      )
    ))
  }
}

imagePoints <- ggproto("imagePoints", Geom,
                       required_aes = c("x", "y", "ids"),
                       default_aes = aes(size = 5, shape=19),
                       
                       draw_key = function (data, params, size) 
                       {
                         iconInfo = data$ids[[1]] # ids contains all the necessary info to render icon
                         legendIcon(iconInfo[[1]], iconInfo[[2]], iconInfo[[3]], iconInfo[[4]])
                       },
                       
                       draw_group = function(data, panel_scales, coord) {
                         coords <- coord$transform(data, panel_scales)
                         grid::pointsGrob(coords$x, coords$y, unit(0, "char"), pch = coords$shape,
                                          gp = grid::gpar(col = "black"))
                       }
)


geom_imagePoint <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, ...) {
  layer(
    geom = imagePoints, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# functions to create the patterns
countGridPoints <- function(x, y, xDiff, yDiff, n = 25){
  xIntervals = as.numeric(cut(x, breaks = seq(xDiff[1], xDiff[2], by=diff(xDiff)/(n-1))))
  yIntervals = as.numeric(cut(y, breaks = seq(yDiff[2], yDiff[1], by=-1*diff(yDiff)/(n-1))))
  yIntervals = n+1-yIntervals
  intervals = as.data.frame(cbind(xIntervals, yIntervals))
  pointsToGrid = as.data.frame(cbind(xIntervals, yIntervals, x, y))
  freqs = count(intervals, vars=c("xIntervals", "yIntervals"))
  freqMat = matrix(0, nrow=n, ncol=n)
  for (x in 1:nrow(freqs)){
    freqMat[freqs$yIntervals[x], freqs$xIntervals[x]] = freqs$freq[x]
  }
 
  xLevels = seq(xDiff[1], xDiff[2], by=diff(xDiff)/(n-1))
  yLevels = seq(yDiff[2], yDiff[1], by=-1*diff(yDiff)/(n-1))
  return(list(xLevels, yLevels, freqMat, pointsToGrid))
}

drawHorizontal <- function(gridOutput, density = 1/2, sparsity = 5, size = 1, xDiff){
  if (density > 1){density=1}
  xBins = gridOutput[[1]]
  yBins = gridOutput[[2]]
  freqMat = gridOutput[[3]]
  pointsToGrid = gridOutput[[4]]
  xStart = c()
  yStart = c()
  xEnd = c()
  yEnd = c()
  adjustmentFactor = 1.75/898 * size * diff(xDiff)
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
        xStart = c(xStart, min(gridPoints$x) - adjustmentFactor) # where to draw horizontal lines
        yStart = c(yStart, yLevels)
        lineDraw = TRUE
      }
      
      if (lineDraw & currentRow[col] == 0 & rowDraw){ # ending line segment
        gridPoints = rowPoints[rowPoints$xIntervals == col-1, ] # points corresponding to current grid square
        xEnd = c(xEnd, max(gridPoints$x) + adjustmentFactor) # where to draw horizontal lines
        yEnd = c(yEnd, yLevels)
        lineDraw = FALSE
      }
      
      # dealing with sparse points
      surroundingGrid = c((col-sparsity):(col-1), (col+1):(col+sparsity))
      surroundingGrid = surroundingGrid[surroundingGrid > 0 & surroundingGrid < ncol(freqMat)] # grids sparsity horizontal distance away
      if (sum(currentRow[surroundingGrid]) == 0 & currentRow[col] != 0 & !lineDraw){ 
        gridPoints = rowPoints[rowPoints$xIntervals == col, ]
        xStart = c(xStart, min(gridPoints$x) - adjustmentFactor) # where to draw horizontal lines
        yStart = c(yStart, mean(gridPoints$y))
        xEnd = c(xEnd, max(gridPoints$x) + adjustmentFactor) # where to draw horizontal lines
        yEnd = c(yEnd, mean(gridPoints$y))
      }
      
      prevCol = currentRow[col]
    }
  }
  
  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}

drawVertical <- function(gridOutput, density = 1/2, sparsity = 5, size = 1, yDiff){
  if (density > 1){density=1}
  xBins = gridOutput[[1]]
  yBins = gridOutput[[2]]
  freqMat = gridOutput[[3]]
  pointsToGrid = gridOutput[[4]]
  xStart = c()
  yStart = c()
  xEnd = c()
  yEnd = c()
  adjustmentFactor = 1.75/560 * size * diff(yDiff)

  colDraw = TRUE # wheater to draw lines in current col or not
  for (col in 1:ncol(freqMat)){ # iterates by every row
    currentCol = freqMat[, col]
    colPoints = pointsToGrid[pointsToGrid$xIntervals == col, ]
    
    if (col+1 <= ncol(freqMat)){  # where to draw x levels
      #xLevels = seq(xBins[col], xBins[col+1], length.out = 3)[2]
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
        yStart = c(yStart, min(gridPoints$y) - adjustmentFactor)
        lineDraw = TRUE
      }
      
      if (lineDraw & currentCol[row] == 0 & colDraw){ # ending line segment
        gridPoints = colPoints[colPoints$yIntervals == row-1, ] # points corresponding to previous grid
        xEnd = c(xEnd, xLevels) # where to draw vertical lines
        yEnd = c(yEnd, max(gridPoints$y) + adjustmentFactor)
        lineDraw = FALSE
      }
      
      # dealing with sparse points
      surroundingGrid = c((row-sparsity):(row-1), (row+1):(row+sparsity))
      surroundingGrid = surroundingGrid[surroundingGrid > 0 & surroundingGrid < nrow(freqMat)] # grids sparsity horizontal distance away
      if (sum(currentCol[surroundingGrid]) == 0 & currentCol[row] != 0 & !lineDraw){ 
        gridPoints = colPoints[colPoints$yIntervals == row, ]
        xStart = c(xStart, mean(gridPoints$x)) # where to draw horizontal lines
        yStart = c(yStart, min(gridPoints$y - adjustmentFactor))
        xEnd = c(xEnd, mean(gridPoints$x)) # where to draw horizontal lines
        yEnd = c(yEnd, max(gridPoints$y + adjustmentFactor))
      }
      
      prevRow = currentCol[row]
    }
  }
  
  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}

drawPositiveDiagnol <- function(gridOutput, density = 1/2, sparsity = 5, size = 1, xDiff){
  if (density > 1){density=1}
  xBins = gridOutput[[1]]
  yBins = gridOutput[[2]]
  freqMat = gridOutput[[3]]
  pointsToGrid = gridOutput[[4]]
  xStart = c()
  yStart = c()
  xEnd = c()
  yEnd = c()
  startingGrids = c(seq(1, nrow(freqMat)^2, by=nrow(freqMat)), seq((nrow(freqMat) * (nrow(freqMat)-1)) + 2, nrow(freqMat)^2)) # which grids diagnol will start
  diagnolDraw = TRUE # wheater to draw in current diagnol or not
  adjustmentFactor = 1.75/898 * size * diff(xDiff)
  for (startingGrid in startingGrids){

    ## deals with determining grids in diagnol
    endingVal = 0 # last grid of diagnol
    if (startingGrid > ((nrow(freqMat)-2)*nrow(freqMat)) + 1){
      endingVal = (startingGrid %% nrow(freqMat)) * nrow(freqMat) 
    }
    
    if (startingGrid == nrow(freqMat)^2){
      endingVal = startingGrid
    }
    diagnol = seq(startingGrid, endingVal, by=-(nrow(freqMat)-1)) # sequence of grids in diagnol
    
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
        xStart = c(xStart, xBins[col])
        yStart = c(yStart, yBins[row])
        lineDraw = TRUE
      }
      
      
      if (lineDraw & currentGridVal == 0 & diagnolDraw){ # when to end a line
        xEnd = c(xEnd, xBins[col])
        yEnd = c(yEnd, yBins[row])
        lineDraw = FALSE
      }
      
      # dealing with sparse points
      surroundingCol= c((col-sparsity):(col-1), (col+1):(col+sparsity))
      surroundingCol = surroundingCol[surroundingCol > 0 & surroundingCol < nrow(freqMat)] # grids sparsity vertical distance away
      
      surroundingRow = c((row-sparsity):(row-1), (row+1):(row+sparsity))
      surroundingRow = surroundingRow[surroundingRow > 0 & surroundingRow< ncol(freqMat)] # grids sparsity vertical distance away
      
      
      if (sum(freqMat[row, surroundingCol]) + sum(freqMat[surroundingRow, col]) == 0 & currentGridVal != 0 & !lineDraw){ 
        
        gridPoints = pointsToGrid[pointsToGrid$yIntervals == row & pointsToGrid$xIntervals == col, ]
        xStart = c(xStart, min(gridPoints$x) - adjustmentFactor)
        xEnd = c(xEnd, max(gridPoints$x) + adjustmentFactor)
        slope = diff(range(yBins))/diff(range(xBins))
        yStart = c(yStart, min(gridPoints$y) - adjustmentFactor)
        yEnd = c(yEnd, (tail(xEnd, 1)-tail(xStart, 1))*slope + min(gridPoints$y) - adjustmentFactor)
      }
      
      prevGrid = currentGridVal
    }
  }
  
  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}

drawNegativeDiagnol <- function(gridOutput, density = 1/2, sparsity = 5, size = 1, xDiff){
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
  adjustmentFactor = 1.75/898 * size * diff(xDiff)

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
        xStart = c(xStart, xBins[col])
        yStart = c(yStart, yBins[row])
        lineDraw = TRUE
      }
      
      
      if (lineDraw & currentGridVal == 0 & diagnolDraw){ # when to end a line
        xEnd = c(xEnd, xBins[col])
        yEnd = c(yEnd, yBins[row])
        lineDraw = FALSE
      }
      
      # dealing with sparse points
      surroundingCol= c((col-sparsity):(col-1), (col+1):(col+sparsity))
      surroundingCol = surroundingCol[surroundingCol > 0 & surroundingCol < nrow(freqMat)] # grids sparsity vertical distance away
      
      surroundingRow = c((row-sparsity):(row-1), (row+1):(row+sparsity))
      surroundingRow = surroundingRow[surroundingRow > 0 & surroundingRow< ncol(freqMat)] # grids sparsity vertical distance away
      
      
      if (sum(freqMat[row, surroundingCol]) + sum(freqMat[surroundingRow, col]) == 0 & currentGridVal != 0 & !lineDraw){ 
        gridPoints = pointsToGrid[pointsToGrid$yIntervals == row & pointsToGrid$xIntervals == col, ]
        xStart = c(xStart, max(gridPoints$x) + adjustmentFactor)
        xEnd = c(xEnd, min(gridPoints$x) - adjustmentFactor)
        slope = diff(range(yBins))/diff(range(xBins))
        yStart = c(yStart, min(gridPoints$y) - adjustmentFactor)
        yEnd = c(yEnd, (tail(xStart, 1)-tail(xEnd, 1))*slope + min(gridPoints$y) - adjustmentFactor)
      }
      
      prevGrid = currentGridVal
    }
  }
  return(data.frame(xStart=xStart, yStart=yStart, xEnd=xEnd, yEnd=yEnd))
}

hatchScatter <- function(data, x, y, factor, factorName, pointSize){
  # colors and patterns to be used
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  patterns <- c('horizontal', 'vertical', 'positiveDiagnol', 'negativeDiagnol')
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
    xGroup = x[factor == group] # gets the points for each group
    yGroup = y[factor == group]
    groupData = data[factor == group, ]
    
    # plot points for each group
    radius = 0.01 * diff(xDiff) * pointSize
    plt = plt + geom_point(data=groupData, x=xGroup, y=yGroup, color=cbbPalette[groupNum], alpha=0.4)
    
    # handles creating the legend icon
    lineType = "solid"
    lineColor = "black"
    legendDF = rbind(legendDF, c(median(xGroup), median(yGroup), group))
    legendIcons[[length(legendIcons) + 1]] = list(cbbPalette[groupNum], lineColor, lineType, patterns[groupNum])
    colnames(legendDF) = names
    
    # get grid of each group
    groupGrid = countGridPoints(xGroup, yGroup, xDiff, yDiff, n=200)
    if (patterns[groupNum] == "horizontal"){
      lineCoords = drawHorizontal(groupGrid, density=1/3, size=pointSize, sparsity=3, xDiff)
    }
    
    if (patterns[groupNum] == "vertical"){
      lineCoords = drawVertical(groupGrid, density=1/3, size=pointSize, sparsity=3, yDiff)
    }
    
    if (patterns[groupNum] == "positiveDiagnol"){
      lineCoords = drawPositiveDiagnol(groupGrid, density=1/3, size=pointSize, sparsity=3, xDiff)
    }
    
    if (patterns[groupNum] == "negativeDiagnol"){
      lineCoords = drawNegativeDiagnol(groupGrid, density=1/3, size=pointSize, sparsity=3, xDiff)
    } 

    plt = plt + geom_segment(data=lineCoords, aes(x=xStart, y=yStart, xend=xEnd, yend=yEnd), alpha=0.4, size=0.75, linetype='solid')
    
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

plt <- hatchScatter(sampleDF, sampleDF$Xt, sampleDF$Yt, as.factor(sampleDF$location), "Tissue Type", 1)
xlabel = "X Coordinates"
ylabel = "Y Coordinates"
title = "Histological View of the PDAC Dataset"
plt = plt + theme_classic() + labs(colour = legend, title=title) + xlab(xlabel) + ylab(ylabel)
plt = plt + theme(plot.title = element_text(family="serif", face="bold", size=25), 
                  axis.title.x = element_text(family="serif", size=20),
                  axis.text.x = element_text(family="serif",color="black", size=15),
                  axis.title.y = element_text(family="serif", size=20),
                  axis.text.y = element_text(family="serif",color="black", size=15))
plt = plt + theme(legend.title = element_text(family="serif", size=20, face="bold"),
                  legend.text = element_text(family="serif", size=15))
plot(plt)



ggsave("D:/umd/summer 2020/hatchEfficientAllPatternsWithLegend.png", dpi=500)



