## ----global.options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align   = 'center'
)

knitr::opts_chunk$set(out.extra = 'style="display:block; margin:auto;"')  # center


## ----setup--------------------------------------------------------------------
library(scatterHatch)

## -----------------------------------------------------------------------------
pdacData = scatterHatch::pdacData
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

head(pdacData[, c('Xt', 'Yt', 'location', 'frame')])

## ----echo=TRUE, message=FALSE, warning=FALSE, tidy=TRUE, cache=FALSE, comment="", dev='svg', fig.width = 9, fig.height = 6.5, fig.align="center"----


plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", 
                    factor = "location", legendTitle = "Tissue Type")
plot(plt)

## ----echo=TRUE, message=FALSE, warning=FALSE, tidy=TRUE, cache=FALSE, comment="", dev='svg', fig.width = 9, fig.height = 6.5, fig.align="center"----


plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", 
                    factor = "frame", pointSize = 0.5, legendTitle = "Frame")
plot(plt)

## ----echo=TRUE, message=FALSE, warning=FALSE, tidy=TRUE, cache=FALSE, comment="", dev='svg', fig.width = 9, fig.height = 6.5, fig.align="center"----
patternList = list(list(pattern="/"), list(pattern="x"), list(pattern=""), list(pattern="-"))
plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", 
                    factor = "location", legendTitle = "Tissue Type", 
                    patternList = patternList)
plot(plt)

## ----echo=TRUE, message=FALSE, warning=FALSE, tidy=TRUE, cache=FALSE, comment="", dev='svg', fig.width = 9, fig.height = 6.5, fig.align="center"----
patternList = list(list(pattern="/", angle = 70), list(pattern="-", density = 1/2), list(pattern="x", angle = c(15,165)), list(pattern="+", density = 1/10))
plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", 
                    factor = "location", legendTitle = "Tissue Type", 
                    patternList = patternList)
plot(plt)

## ----echo=FALSE---------------------------------------------------------------
parameters = c('data', 'x', 'y', 'factor', 'legendTitle', 'pointSize', 'pointAlpha', 'gridSize', 'sparsePoints', 'patternList', 'colorPalette')
paramDescript = c('A dataframe of the dataset being plotted', 'A string describing the column name with the x-coordinates of the points being plotted', 'A string describing the column name with the y-coordinates of the points being plotted', 'A string describing the column name of the factor variable', 'The legend title', 'ggplot2 point size', 'Transparency of each point', 'Integer describing the precision of the hatched patterns.  Larger the value, greater the precision at the expense of efficiency.  Default follows this expotential decay function: 500*e^(-pointSize/2.2) + 43.44965', 'A logical vector that denotes the outlying points.  Default utilizies an in-built sparsity detector', 'List containing the aesthethics of each pattern', 'Character vector describing the point color of each group; default is color-blind friendly and uses colors from the dittoSeq package')

paramTable = data.frame(parameters, paramDescript)
knitr::kable(paramTable, col.names = c("Argument","Description"))


## ----echo=FALSE---------------------------------------------------------------
aesthe = c('pattern', 'density', 'angle')
aestheDescript = c('A string representing one of the possible 7 patterns to be used: (horizontal or "-"), (vertical or "|"), (positiveDiagonal or "/"), (negativeDiagonal or "\\"), (checkers or "+"), (cross or "x"), and (blank or "").', 'A fraction representing how dense the lines in a pattern should be.  Must be less than or equal to 1, with the denominator as an integer and the numerator the number 1.  Greater the density value, denser the pattern.', 'Vector or number denoting at what angle(s) the lines in a hatching pattern should be drawn.')

aestheTable = data.frame(aesthe, aestheDescript)
knitr::kable(aestheTable, col.names = c("Aesthetics","Description"))

