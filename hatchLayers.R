library(ggplot2)
library(png)
library(grid)
library(magick)
library(imager)
library(stringi)
library(rsvg)


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
imagePoints <- ggproto("imagePoints", Geom,
                       required_aes = c("x", "y", "image"),
                       default_aes = aes(size = 5, shape=19),
                       
                       draw_key = function (data, params, size) 
                       {
                         grid::rasterGrob(0.5, 0.5, image = image_read_svg(data$image, width=160), width = 1, height = 1)
                       },
                       
                       draw_group = function(data, panel_scales, coord) {
                         coords <- coord$transform(data, panel_scales)
                         grid::pointsGrob(coords$x, coords$y, unit(0, "char"), pch = coords$shape,
                                          gp = grid::gpar(col = coords$colour))
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


# turn non point pixels in graph transparent and turn white pixels to a specified color

turnPixelTransparent <- function(pointRaster, patternImage, color, rotate){
  # loading in pattern image
  img <- rsvg(patternImage, width=ncol(pointRaster), height=nrow(pointRaster))
  rgbColor = col2rgb(color)
  redScale <- img[,,1]
  greenScale <- img[,,1]
  blueScale <- img[,,1]
  alphaScale <- matrix(1, nrow = nrow(pointRaster), ncol = ncol(pointRaster))
  
  # replaces white with respective color values for each channel
  
  redScale[redScale != 1] <- 0
  blueScale[blueScale != 1] <- 0
  greenScale[greenScale != 1] <- 0
  redScale[redScale != 0] <- rgbColor["red",]/255
  blueScale[blueScale != 0] <- rgbColor["blue",]/255
  greenScale[greenScale != 0] <- rgbColor["green",]/255
  
  # turns background pixels in pattern transparent
  
  nonpoints = pointRaster[,,1]
  alphaScale[nonpoints == 1] <- 0
  
  newimg <- array(0, dim=c(nrow(pointRaster), ncol(pointRaster), 4))
  newimg[,,1:4] = c(redScale, greenScale, blueScale, alphaScale)
  
  # creates a picture to be used for a legend
  legendimg <- array(1, dim=c(nrow(pointRaster), ncol(pointRaster), 3)) 
  legendimg[,,1:3] = c(redScale, greenScale, blueScale)
  legendimg <- suppressWarnings(as.cimg(legendimg))
  
  # turns into a circle
  Xcc <- function(im){ Xc(legendimg) - width(legendimg)/2}
  Ycc <- function(im){ Yc(legendimg) - height(legendimg)/2}
  px <- ((Xcc(legendimg))^2 + (Ycc(legendimg))^2) > 80^2
  legendimg<-colorise(legendimg,px,"white")
  legendimg <- image_read(as.raster(legendimg))
  legendimg <- image_trim(legendimg)
  legendimg <- image_transparent(legendimg, "white")
  legendimg <- image_rotate(legendimg, rotate)
  
  
  fileName <- tail(strsplit(patternImage, "/")[[1]], 1)
  image_write(legendimg, path = paste0("D:/umd/summer 2020/legend/", fileName), format="svg")
  
  return(newimg)
}

hatchScatter <- function(data, x, y, factor, factorName){
  # colors and patterns to be used
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  patterns <- c("D:/umd/summer 2020/patterns/cross.svg","D:/umd/summer 2020/patterns/horizontal.svg","D:/umd/summer 2020/patterns/vertical.svg", "D:/umd/summer 2020/patterns/diagnol.svg", "D:/umd/summer 2020/patterns/positivediagnol.svg")
  rotate <- c(90, 90, 90, 180, -90) # indicates wheather how much a legend image needs to be rotated  to match hatching pattern
    
  # figuring out how many groups present
  groupNum = 1
  groups <- levels(factor)
  
  # creating the master plot
  plt = ggplot(data=data, aes(x=x, y=y))
  xDiff = ggplot_build(plt)$layout$panel_params[[1]]$x.range # gets x range
  yDiff = ggplot_build(plt)$layout$panel_params[[1]]$y.range # gets y range
  
  # creating the rasterized patterns for each group
  for (group in groups){
    xGroup = x[factor == group] # gets the points for each group
    yGroup = y[factor == group]
    groupData = data[factor == group, ]
    rasterPlot = ggplot(data=groupData, aes(x=xGroup, y=yGroup)) + geom_point() + theme_void() + lims(x=xDiff, y=yDiff)
    plot(rasterPlot)
    ggsave(plot=rasterPlot, "D:/umd/summer 2020/rasterPoints.png", device = "png") #saves ggplot of one group
    rasterPoints = readPNG("D:/umd/summer 2020/rasterPoints.png")

    # use ggplot of one group to make rasterized image of pattern
    patternPoints = turnPixelTransparent(rasterPoints, patterns[groupNum], cbbPalette[groupNum], rotate[groupNum])
    
    # add rasterized image of pattern to the main plot
    plt = plt + annotation_custom(rasterGrob(patternPoints, width=unit(1,"npc"), height=unit(1,"npc"))) 
    groupNum = groupNum + 1
  }
  
  # drawing the legend
  legendImageFiles = sapply(patterns[1:groupNum-1], function(x){paste0("D:/umd/summer 2020/legend/", tail(strsplit(x, "/")[[1]], 1))})
  spoofDF = data.frame(x=max(x)/2, y=max(y)/2, image=legendImageFiles, location=levels(factor))
  scale_image <- function(..., guide="legend"){ # adding in the factor names and image paths to legend
    scale_discrete_manual(aes="image", labels=levels(factor), values=unname(legendImageFiles))
  } 
  plt <- plt + geom_imagePoint(data=spoofDF, aes(x=x, y=y, image=image))+ scale_image()
  plt$labels$image <- factorName
  
  # ensures axis not impacted by imagePoint layer
  plt = plt + lims(x=xDiff, y=yDiff)
  return(plt)
}

plt <- hatchScatter(sampleDF, sampleDF$Xt, sampleDF$Yt, as.factor(sampleDF$location), "Tissue Type")
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



