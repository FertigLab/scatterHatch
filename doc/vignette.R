## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(scatterHatch)
library(ggplot2)

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

head(pdacData[, c('Xt', 'Yt', 'location')])

## ----echo=TRUE, message=FALSE, warning=FALSE, tidy=TRUE, cache=FALSE, comment="", fig.width=7, fig.height=4.365256----
myTheme <- theme_classic() + theme(plot.title = element_text(family="serif", face="bold", size=25), 
                  axis.title.x = element_text(family="serif", size=20),
                  axis.text.x = element_text(family="serif",color="black", size=15),
                  axis.title.y = element_text(family="serif", size=20),
                  axis.text.y = element_text(family="serif",color="black", size=15)) +
                  theme(legend.title = element_text(family="serif", size=20, face="bold"),
                  legend.text = element_text(family="serif", size=15))

plt <- scatterHatch(data = pdacData, x = pdacData$Xt, y = pdacData$Yt, 
                    factor = as.factor(pdacData$location), factorName = "Tissue Type", pointSize = 1) + myTheme
plot(plt)

## ----echo=TRUE, message=FALSE, warning=FALSE, tidy=TRUE, cache=FALSE, comment="", fig.width=8, fig.height=4.988864----
patternList = list(list(pattern="positiveDiagonal"), list(pattern="horizontal"), list(pattern="negativeDiagonal"), list(pattern="vertical"))
plt <- scatterHatch(data = pdacData, x = pdacData$Xt, y = pdacData$Yt, 
                    factor = as.factor(pdacData$location), factorName = "Tissue Type", pointSize = 1, patternList = patternList) + myTheme
plot(plt)

## ----echo=TRUE, message=FALSE, warning=FALSE, tidy=TRUE, cache=FALSE, comment="", fig.width=8, fig.height=4.988864----
patternList = list(list(pattern="positiveDiagonal", lineColor="red"), list(pattern="vertical", density=1/8))
plt <- scatterHatch(data = pdacData, x = pdacData$Xt, y = pdacData$Yt, factor = as.factor(pdacData$location), factorName = "Tissue Type", pointSize = 1, patternList = patternList) + myTheme
plot(plt)

## ----echo=FALSE---------------------------------------------------------------
parameters = c('data', 'x', 'y', 'factor', 'factorName', 'pointSize', 'gridSize', 'patternList', 'colorPalette')
paramDescript = c('A dataframe of the dataset being plotted', 'A numeric vector describing the x-coordinates of the points being plotted', 'A numeric vector describing the y-coordinates of the points being plotted', 'A vector describing what group each point belongs to', 'The legend title', 'ggplot2 point size', 'Integer describing the precision of the hatched patterns.  Larger the value, greater the precision at the expense of efficiency.  Default follows this expotential decay function: 500*e^(-pointSize/2.2) + 43.44965', 'List containing the aesthethics of each pattern', 'Character vector describing the point color of each group; default is color-blind friendly')

paramTable = data.frame(parameters, paramDescript)
knitr::kable(paramTable, col.names = c("Argument","Description"))

myTheme <- theme_classic() + theme(plot.title = element_text(family="serif", face="bold", size=25), 
                  axis.title.x = element_text(family="serif", size=20),
                  axis.text.x = element_text(family="serif",color="black", size=15),
                  axis.title.y = element_text(family="serif", size=20),
                  axis.text.y = element_text(family="serif",color="black", size=15)) +
                  theme(legend.title = element_text(family="serif", size=20, face="bold"),
                  legend.text = element_text(family="serif", size=15))

## ----echo=FALSE---------------------------------------------------------------
aesthe = c('pattern', 'density', 'sparsity', 'lineColor', 'lineType', 'lineWidth', 'pointAlpha')
aestheDescript = c('A string representing which pattern to use ("horizontal", "vertical", "positiveDiagonal", "negativeDiagonal")', 'A fraction representing how dense the lines in a pattern should be.  Must be less than or equal to 1, with the denominator as an integer and the numerator the number 1.  Greater the density value, denser the pattern. Default is 1/4', 'Value controlling the requirements for a point to be labelled as sparse and have an individual line pattern be rendered on it.  Less the sparsity value, less the requirements.  Value >= 1 and <= gridSize.  Default is 2% of the gridSize', 'Color of the pattern line', 'Type of line (e.g. dashed, dotted, etc.)',
                  'Width of the lines in the pattern',
                  'Alpha value of the points being plotted for each group')

aestheTable = data.frame(aesthe, aestheDescript)
knitr::kable(aestheTable, col.names = c("Aesthetics","Description"))

