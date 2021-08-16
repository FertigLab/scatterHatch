#' Gets Dataframes of sparse/small cluster points in a group
#' @param pointsToGrid Dataframe corresponding each point to a grid
#' @param freqMat Matrix denoting how many points within a grid
#' @param sparsePoints Logical vector of sparse points
#' @param rotatedxRange x-range of plot rotated based on angle of pattern
#' @param rotatedyRange y-range of plot rotated based on angle of pattern
#' @param xRange x-range of plot
#' @param pointSize Size of points in scatterHatch
#' @noRd
getIrregularPoints <- function(pointsToGrid, freqMat, sparsePoints, 
                               rotatedxRange, rotatedyRange, xRange, pointSize){
  
    sparsityAnnotateOutput <- sparsityAnnotate(pointsToGrid, pointSize, 
                                            rotatedxRange, rotatedyRange, 'x')
  
    ## if sparse points are not given
    if (is.null(sparsePoints)){ 
        sparsePoints <- sparsityAnnotateOutput$sparsePoints
    }
  
    sparsePointsToGrid <- pointsToGrid[sparsePoints, ]
    smallClusterToGrid <- pointsToGrid[sparsityAnnotateOutput$smallClusters, ]
  
    ## removes sparse and small cluster points from regular pattern drawing
    pointsToGrid <- pointsToGrid[!sparsePoints & 
                                !sparsityAnnotateOutput$smallClusters, ]
  
    ## removes sparse points from 2D frequency matrix
    allIrregularPoints <- rbind(sparsePointsToGrid, smallClusterToGrid)
    for (i in seq(nrow(allIrregularPoints))){ 
        freqMat[allIrregularPoints$yIntervals[i], allIrregularPoints$xIntervals[i]] = freqMat[allIrregularPoints$yIntervals[i], allIrregularPoints$xIntervals[i]] - 1
    }
  
    return(list(sparsePointsToGrid, smallClusterToGrid, pointsToGrid, freqMat))
}

#' Finds the line segments for drawing patterns in large clusters
#' @param freqMat Matrix denoting how many points within a grid
#' @param pointsToGrid Dataframe corresponding each point to a grid
#' @param yBins y-coordinate bins for every grid
#' @param density Density of pattern drawing (how many rows to skip)
#' @noRd
regularPatternDraw <- function(freqMat, pointsToGrid, yBins, density){
    xStart <- yStart <- xEnd <- yEnd <- c()
    rowDraw <- TRUE # whether to draw lines in current row or not
  
    for (row in seq(nrow(freqMat))){ # iterates by every row
        rowPoints <- pointsToGrid[pointsToGrid$yIntervals == row, ]
    
        yLevels <- yBins[row] - diff(yBins)[1]/2 # atul's version
        if (row == nrow(freqMat)){ # for bottom row exception
            yLevels <- yBins[row] - diff(yBins)[1]/2
        }
    
        rowDraw <- (as.integer(row * density)*(1/density)) == row #when to skip
    
        prevCol <- 0
        lineDraw <- FALSE # whether line being drawn or not
    
        for (col in seq(ncol(freqMat))){
            ## starting a line segment
            if (prevCol == 0 & freqMat[row, col] != 0  & rowDraw){ 
                gridPoints <- rowPoints[rowPoints$xIntervals == col, ]
                xStart <- c(xStart, min(gridPoints$x))
                yStart <- c(yStart, yLevels)
                lineDraw <- TRUE
            }
      
            ## ending line segment
            if (lineDraw & freqMat[row, col] == 0 & rowDraw){ 
                gridPoints <- rowPoints[rowPoints$xIntervals == col-1, ]
                xEnd <- c(xEnd, max(gridPoints$x))
                yEnd <- c(yEnd, yLevels)
                lineDraw <- FALSE
            }
      
            prevCol <- freqMat[row, col]
        }
    }
  
    return(list(xStart, xEnd, yStart, yEnd))
  }