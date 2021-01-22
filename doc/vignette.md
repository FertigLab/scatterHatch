## Importing Local Libraries

``` r
library(scatterHatch)
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

head(pdacData[, c('Xt', 'Yt', 'location', 'frame')])
#>         Xt        Yt location frame
#> 1 1342.878  -801.154 Pancreas     1
#> 2 5688.494 -1391.393 Pancreas     4
#> 3 6295.826 -1393.807 Pancreas     4
#> 4 5344.257 -1391.650 Pancreas     4
#> 5 5640.034 -1391.416 Pancreas     4
#> 6 5923.357 -1390.776 Pancreas     4
```

## Creating a Basic ScatterHatch Plot

`scatterHatch()` must have a data frame passed to it, along with three
strings denoting the columns with the x/y coordinates and a factor
variable of each point being plotted. The factor variable identifies the
group each point is a part of. `scatterHatch()` returns a ggplot2 object
with three layers; the points, the line segments (the hatching), and an
invisible custom geom to render the legend icons.

``` r


plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", factor = "location", legendTitle = "Tissue Type")
[1] 0.25
[1] 0.3333333
[1] 0.3333333
plot(plt)
```

<img src="C:/Users/guhat/AppData/Local/Temp/RtmpM3p5xl/preview-56bc49ef1a32.dir/vignette_files/figure-gfm/unnamed-chunk-2-1.svg" style="display:block; margin:auto;" style="display: block; margin: auto;" />

## Creating ScatterHatch Plot with Many Groups

``` r


plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", factor = "frame", pointSize = 0.5, 
    legendTitle = "Frame")
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.25
[1] 0.25
[1] 0.25
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
plot(plt)
```

<img src="C:/Users/guhat/AppData/Local/Temp/RtmpM3p5xl/preview-56bc49ef1a32.dir/vignette_files/figure-gfm/unnamed-chunk-3-1.svg" style="display:block; margin:auto;" style="display: block; margin: auto;" />

## Customizing ScatterHatch Plot

### Changing the Order of Pattern Assignment

Controlling the aesthetics of each pattern is done by passing a list to
the `patternList` argument. Each element of the list should contain a
list that denotes the desired aesthetics for each pattern. Every element
of `patternList` must have a named element called “pattern” that
contains the unique pattern type for which the aesthetics are being
changed. If patternList is passed, then the length of patternList must
be equal to the number of groups being plotted.

``` r
patternList = list(list(pattern = "/"), list(pattern = "x"), list(pattern = ""), 
    list(pattern = "-"))
plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", factor = "location", legendTitle = "Tissue Type", 
    patternList = patternList)
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
plot(plt)
```

<img src="C:/Users/guhat/AppData/Local/Temp/RtmpM3p5xl/preview-56bc49ef1a32.dir/vignette_files/figure-gfm/unnamed-chunk-4-1.svg" style="display:block; margin:auto;" style="display: block; margin: auto;" />

### Changing the Aesthetics of each Pattern

There are currently two aesthetics of each pattern that can be changed:
density and angle. Density denotes the density of the hatching for a
pattern. The densest a pattern can be is 1 and must be a reciprocal of
an integer (e.g. 1/4, 1/2, 1/3, etc.). Angle denotes the angle(s) for
which the lines of a particular hatching pattern should be drawn. For
example, the default horizontal pattern (“+”) is an angle of 0 while the
default vertical pattern (“-”) is an angle of 90. Angle can be a vector
with multiple angles. For example, the cross (“x”) is an angle of 45 and
135.

``` r
patternList = list(list(pattern = "/", angle = 70), list(pattern = "-", density = 1/2), 
    list(pattern = "x", angle = c(15, 165)), list(pattern = "+", density = 1/10))
plt <- scatterHatch(data = pdacData, x = "Xt", y = "Yt", factor = "location", legendTitle = "Tissue Type", 
    patternList = patternList)
[1] 0.3333333
[1] 0.3333333
[1] 0.3333333
[1] 0.1
[1] 0.1
plot(plt)
```

<img src="C:/Users/guhat/AppData/Local/Temp/RtmpM3p5xl/preview-56bc49ef1a32.dir/vignette_files/figure-gfm/unnamed-chunk-5-1.svg" style="display:block; margin:auto;" style="display: block; margin: auto;" />

## scatterHatch() Arguments Explained

| Argument     | Description                                                                                                                                                                                                         |
| :----------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| data         | A dataframe of the dataset being plotted                                                                                                                                                                            |
| x            | A string describing the column name with the x-coordinates of the points being plotted                                                                                                                              |
| y            | A string describing the column name with the y-coordinates of the points being plotted                                                                                                                              |
| factor       | A string describing the column name of the factor variable                                                                                                                                                          |
| legendTitle  | The legend title                                                                                                                                                                                                    |
| pointSize    | ggplot2 point size                                                                                                                                                                                                  |
| pointAlpha   | Transparency of each point                                                                                                                                                                                          |
| gridSize     | Integer describing the precision of the hatched patterns. Larger the value, greater the precision at the expense of efficiency. Default follows this expotential decay function: 500\*e^(-pointSize/2.2) + 43.44965 |
| sparsePoints | A logical vector that denotes the outlying points. Default utilizies an in-built sparsity detector                                                                                                                  |
| patternList  | List containing the aesthethics of each pattern                                                                                                                                                                     |
| colorPalette | Character vector describing the point color of each group; default is color-blind friendly and uses colors from the dittoSeq package                                                                                |

## Pattern Aesthetics Arguments

These pattern aesthetics are passed into a list in the patternList
argument of `scatterHatch()`.

| Aesthetics | Description                                                                                                                                                                                                            |
| :--------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| pattern    | A string representing one of the possible 7 patterns to be used: (horizontal or “-”), (vertical or “|”), (positiveDiagonal or “/”), (negativeDiagonal or “"), (checkers or”+“), (cross or”x“), and (blank or”").       |
| density    | A fraction representing how dense the lines in a pattern should be. Must be less than or equal to 1, with the denominator as an integer and the numerator the number 1. Greater the density value, denser the pattern. |
| angle      | Vector or number denoting at what angle(s) the lines in a hatching pattern should be drawn.                                                                                                                            |
