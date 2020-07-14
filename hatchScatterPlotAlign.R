library(imager)
library(magick)
library(ggplot2)
library(plyr)
library(png)
library(ggimg)



offset <- function(x, y){ # calculates the values a point is to alignment
  positiveVal = x %% y
  if (abs(positiveVal - y) < positiveVal){
    return(positiveVal - y)
  }
  return(positiveVal)
}

circler <- function(img, x, y, size, xRange, yRange){ #turns an image into a circle with transparent background and aligns
  
  # eliminate transparency channel if present
  if (dim(img)[4] > 3){
    img <- as.cimg(img[,,,1:3])
  }
  
  # creates new coord sys and cuts circle to create uniformity in hatch plot
  centerX = width(img)/2
  centerY = height(img)/2
  center <- paste0("120x120+",as.character(centerX/2),"+",as.character(centerY/2))
  Xcc <- function(im){ Xc(img) - width(img)/2}
  Ycc <- function(im){ Yc(img) - height(img)/2}
  
  # calculating how much to offset the circle to align image with other point images
  xSize = 24/898 * size * xRange # range of x values a point image takes on a graph
  ySize = 24/560 * size * yRange # range of y values a point image takes on a graph
  xoffSetFactor = offset(x, xSize)
  yoffSetFactor = offset(y, ySize)
  xOffSet = xoffSetFactor/xSize * 60 # converts x-val offset value to image coordinates
  yOffSet = yoffSetFactor/ySize * 60 # converts y-val offset value to image coordinates

  # turning image to a circle with transparent background
  px <- ((Xcc(img) - xOffSet)^2 + (Ycc(img))^2) > 60^2 # circle equation
  img<-colorise(img,px,"grey")
  mag <- image_read(as.raster(img))
  mag <- image_trim(mag)
  mag <- image_transparent(mag, "grey")
  
  imgData <- as.numeric(image_data(mag, "rgba")) # returns image as a raster
  return(imgData)
}

colorChange <- function(target, color){ # turns white color in circle images to any color
  
  # starts off with only RGBA channels (but black and white)
  
  img = target
  rgbColor = col2rgb(color)
  redScale <- img[,,1]
  greenScale <- img[,,1]
  blueScale <- img[,,1]
  
  # replaces white with respective color values for each channel
  
  redScale[redScale != 0] <- rgbColor["red",]/255 
  blueScale[blueScale != 0] <- rgbColor["blue",]/255
  greenScale[greenScale != 0] <- rgbColor["green",]/255
  alphaScale <- img[,,4]
  dimImg = dim(img)
  img = array(0, dim=c(dimImg[1],dimImg[2],4))
  img[,,1:4] = c(redScale, greenScale, blueScale, alphaScale)
  return(img)
}



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

sampleDF = pdacData[sample(nrow(pdacData), 1000),] # random sample of 1000 points from PDAC dataset


size = 0.5 # point size
plt = ggplot(sampleDF, aes(x=Xt, y=Yt)) # first need to get the x and y vals into plot
xDiff = diff(ggplot_build(plt)$layout$panel_params[[1]]$x.range) # figure out range of vals
yDiff = diff(ggplot_build(plt)$layout$panel_params[[1]]$y.range)

# calculate raster object for each point and store to images
images <- lapply(1:nrow(sampleDF), function(x){circler(load.image("D:/umd/summer 2020/patterns/vertical3.png"), sampleDF$Xt[x], sampleDF$Yt[x], size, xDiff, yDiff)})

# plot graph
plt = plt + geom_point_img(aes(img=images), size=size, alpha=-1) + theme_minimal()
plot(plt)
ggsave("D:/umd/summer 2020/alignFourth.png")



