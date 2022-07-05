#' Gets Dataframes of sparse/small cluster points in a group
#' @param pointsToGrid Dataframe corresponding each point to a grid
#' @param freqMat Matrix denoting how many points within a grid
#' @param sparsePoints Logical vector of sparse points
#' @param rotatedxRange x-range of plot rotated based on angle of pattern
#' @param rotatedyRange y-range of plot rotated based on angle of pattern
#' @param pointSize Size of points in scatterHatch
#' @noRd
getIrregularPoints <- function(pointsToGrid, freqMat, sparsePoints, 
    rotatedxRange, rotatedyRange, pointSize){
    
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
#' @noRd
regularPatternDraw <- function(freqMat, pointsToGrid, yBins){
    xStart <- yStart <- xEnd <- yEnd <- c()
    for (rowNum in seq(nrow(freqMat))){ # iterates by every rowNum
        rowPoints <- pointsToGrid[pointsToGrid$yIntervals == rowNum, ]
        
        yLevels <- yBins[rowNum] + diff(yBins)[1]/2 # atul's version
        if (rowNum == nrow(freqMat)){ # for bottom rowNum exception
            yLevels <- yBins[rowNum] + diff(yBins)[1]/2
        }
        
        prevCol <- 0
        lineDraw <- FALSE # whether line being drawn or not
        
        for (colNum in seq(ncol(freqMat))){
            ## starting a line segment
            if (prevCol == 0 & freqMat[rowNum, colNum]!=0){ 
                gridPoints <- rowPoints[rowPoints$xIntervals == colNum, ]
                gridPoints <- gridPoints[abs(gridPoints$y-yLevels)<(diff(yBins)[1]/8),] # 
                xStart <- c(xStart, min(gridPoints$x))
                yStart <- c(yStart, yLevels)
                lineDraw <- TRUE
            }
            
            ## ending line segment (added logic for handling end of freqMat before end of points)
            if (lineDraw & (freqMat[rowNum, colNum]==0||colNum==ncol(freqMat))){ 
                if (freqMat[rowNum, colNum]==0)
                    gridPoints <- rowPoints[rowPoints$xIntervals == colNum-1, ]
                else
                    gridPoints <- rowPoints[rowPoints$xIntervals == colNum, ]
                gridPoints <- gridPoints[abs(gridPoints$y-yLevels)<(diff(yBins)[1]/8),] # 
                xEnd <- c(xEnd, max(gridPoints$x))
                yEnd <- c(yEnd, yLevels)
                lineDraw <- FALSE
            }
            
            prevCol <- freqMat[rowNum, colNum]
        }
    }
    
    return(data.frame(xStart=xStart, xEnd=xEnd, yStart=yStart, yEnd=yEnd))
}
