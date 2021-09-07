# scatterHatch
The goal of scatterHatch is to create scatter plots that visually distinguish groups of data using color and texture in sparse or dense datasets.

## Why you need scatterHatch?
It is often said that a picture is worth a thousand words.  In bioinformatics, visualizations attempt to concisely convey complex relationships between a multitude of factors and groups.  One could argue these visualizations are worth a lot more than a thousand words.  However, it is important to remember there is a balance between encapsulating large amounts of information in a visualization, and correctly deciphering that same information from a visualization.  This balance is not the same for everyone in an audience.  

In bioinformatics, scatterplots often contain many groups represented by many colors.  Distinguishing among unique groups can be a difficult task for colorblind individuals. This package provides the ```scatterHatch``` function that adds a texture to each group in a scatterplot so that colorblind individuals can distinguish groups using two characteristics instead of one.

## Installation
You can install scatterHatch using the [Bioconductor project](https://bioconductor.org/).

```
install.packages("BiocManager")
BiocManager::install("scatterHatch")
```

You can also install scatterHatch directly from the Github source.
```
install.packages("remotes")
remotes::install_github("FertigLab/scatterHatchPlots", dependencies = TRUE, build_vignettes = TRUE)
```

## Vignettes
To learn how to use the scatterHatch package, run the vignette below in `R`.

* [scatterHatch Vignette](https://github.com/FertigLab/scatterHatch/blob/master/vignettes/vignette.Rmd)
