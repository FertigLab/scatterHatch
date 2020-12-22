Creating a Scatterplot with Texture
================
Tejas Guha
October 1, 2020

## Importing Local Libraries

``` r
library(scatterHatch)
library(ggplot2)
```

## Preparing the Data

The data that will be used to showcase the function is [a tissue-CyCIF
PDAC dataset from Lin et
al](http://spatial.rc.fas.harvard.edu/spatialgiotto/giotto.cycif.html).
The preprocessing begins by adding manual annotations for each cell’s
location in the tissue sample.

``` r
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
#>         Xt        Yt location
#> 1 1342.878  -801.154 Pancreas
#> 2 5688.494 -1391.393 Pancreas
#> 3 6295.826 -1393.807 Pancreas
#> 4 5344.257 -1391.650 Pancreas
#> 5 5640.034 -1391.416 Pancreas
#> 6 5923.357 -1390.776 Pancreas
```

## Creating a Basic ScatterHatch Plot

`scatterHatch()` must have a data frame passed to it, along with two
vectors denoting the x and y coordinates of each point being plotted.
Additionally, the factor argument must have a factor vector that
identifies the group each point is part of. Lastly, factorName is the
title of the legend for the scatterplot. `scatterHatch()` returns a
ggplot2 object where each line texture is represented as a
`geom_segment()`.

``` r
myTheme <- theme_classic() + theme(plot.title = element_text(family = "serif", face = "bold",
    size = 25), axis.title.x = element_text(family = "serif", size = 20), axis.text.x = element_text(family = "serif",
    color = "black", size = 15), axis.title.y = element_text(family = "serif", size = 20),
    axis.text.y = element_text(family = "serif", color = "black", size = 15)) + theme(legend.title = element_text(family = "serif",
    size = 20, face = "bold"), legend.text = element_text(family = "serif", size = 15))

plt <- scatterHatch(data = pdacData, x = pdacData$Xt, y = pdacData$Yt, factor = as.factor(pdacData$location),
    factorName = "Tissue Type", pointSize = 1) + myTheme
plot(plt)
```

![](https://github.com/FertigLab/scatterHatchPlots/blob/master/doc/vignette_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Customizing ScatterHatch Plot

### Changing the Order of Pattern Assignment

Controlling the aesthetics of each pattern is done by passing a list to
the `patternList` argument. Each element of the list should contain a
list that denotes the desired aesthetics for each pattern. Every element
of `patternList` must have a named element called “pattern” that
contains the unique pattern type for which the aesthetics are being
changed. If the length of `patternList` is equal to the number of groups
in `factor` then pattern assignment for each group will be based on the
order of patterns in `patternList`. Below, the first group, “Pancreas”,
uses the aesthetics of the first element in \``patternList` -
positiveDiagnol - instead of horizontal.

``` r
patternList = list(list(pattern = "positiveDiagonal"), list(pattern = "horizontal"),
    list(pattern = "negativeDiagonal"), list(pattern = "vertical"))
plt <- scatterHatch(data = pdacData, x = pdacData$Xt, y = pdacData$Yt, factor = as.factor(pdacData$location),
    factorName = "Tissue Type", pointSize = 1, patternList = patternList) + myTheme
plot(plt)
```

![](https://github.com/FertigLab/scatterHatchPlots/blob/master/doc/vignette_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Changing the Aesthetics of each Pattern

Changing the aesthetics of each pattern, like line color or line size,
is done by adding a named element for the respective aesthetic being
changed and the value desired. Note, pattern assignment for each group
is unaffected when the number of groups in `factor` is greater than the
number of elements in `patternList`. To make the positiveDiagonal
pattern have red lines and the vertical pattern have a decreased line
density:

``` r
patternList = list(list(pattern = "positiveDiagonal", lineColor = "red"), list(pattern = "vertical",
    density = 1/8))
plt <- scatterHatch(data = pdacData, x = pdacData$Xt, y = pdacData$Yt, factor = as.factor(pdacData$location),
    factorName = "Tissue Type", pointSize = 1, patternList = patternList) + myTheme
plot(plt)
```

![](https://github.com/FertigLab/scatterHatchPlots/blob/master/doc/vignette_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## scatterHatch() Arguments Explained

| Argument     | Description                                                                                                                                                                                                         |
| :----------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| data         | A dataframe of the dataset being plotted                                                                                                                                                                            |
| x            | A numeric vector describing the x-coordinates of the points being plotted                                                                                                                                           |
| y            | A numeric vector describing the y-coordinates of the points being plotted                                                                                                                                           |
| factor       | A vector describing what group each point belongs to                                                                                                                                                                |
| factorName   | The legend title                                                                                                                                                                                                    |
| pointSize    | ggplot2 point size                                                                                                                                                                                                  |
| gridSize     | Integer describing the precision of the hatched patterns. Larger the value, greater the precision at the expense of efficiency. Default follows this expotential decay function: 500\*e^(-pointSize/2.2) + 43.44965 |
| patternList  | List containing the aesthethics of each pattern                                                                                                                                                                     |
| colorPalette | Character vector describing the point color of each group; default is color-blind friendly                                                                                                                          |

## Pattern Aesthetics Arguments

These pattern aesthetics are passed into a list in the patternList
argument of `scatterHatch()`.

| Aesthetics | Description                                                                                                                                                                                                                                |
| :--------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| pattern    | A string representing which pattern to use (“horizontal”, “vertical”, “positiveDiagonal”, “negativeDiagonal”)                                                                                                                              |
| density    | A fraction representing how dense the lines in a pattern should be. Must be less than or equal to 1, with the denominator as an integer and the numerator the number 1. Greater the density value, denser the pattern. Default is 1/4      |
| sparsity   | Value controlling the requirements for a point to be labelled as sparse and have an individual line pattern be rendered on it. Less the sparsity value, less the requirements. Value \>= 1 and \<= gridSize. Default is 2% of the gridSize |
| lineColor  | Color of the pattern line                                                                                                                                                                                                                  |
| lineType   | Type of line (e.g. dashed, dotted, etc.)                                                                                                                                                                                                   |
| lineWidth  | Width of the lines in the pattern                                                                                                                                                                                                          |
| pointAlpha | Alpha value of the points being plotted for each group                                                                                                                                                                                     |
