library(ggimage)
library(imager)
library(magick)
library(ggplot2)
library(Cairo)

circler <- function(img, filePath){ #turns an image into a circle with transparent background
  # eliminate transparency channel if present
  if (dim(img)[4] > 3){
    img <- as.cimg(img[,,,1:3])
  }
  
  # creates new coord sys centered at center of image
  centerX = width(img)/2
  centerY = height(img)/2
  Xcc <- function(im) Xc(img) - width(img)/2
  Ycc <- function(im) Yc(img) - height(img)/2
  
  # circle equation
  px <- (Xcc(img)^2+Ycc(img)^2) > 60^2
  img<-colorise(img,px,"grey")
  
  # use magick to make everything outside circle transparent
  mag <- image_read(as.raster(img))
  mag <- image_transparent(mag, "grey")
  plot(mag)
  image_write(mag, filePath)


}

files <- c('checker', 'crisscross', 'horizontal', 'vertical', 'polka') #files name with pattern images
for (i in files){
  img <- load.image(paste0("patterns/", i, ".png"))
  circler(img, paste0("D:/umd/summer 2020/patterns/", i, "Circle.png"))
}



colorChange <- function(target, color){ # turns white color in circle images to any color
  
  # starts off with only Gray-Alpha channels
  
  img = readPNG(target) 
  rgbColor = col2rgb(color)
  redScale <- img[,,1]
  greenScale <- img[,,1]
  blueScale <- img[,,1]
  
  # replaces white with respective color values for each channel
  
  redScale[redScale != 0] <- rgbColor["red",]/255 
  blueScale[blueScale != 0] <- rgbColor["blue",]/255
  greenScale[greenScale != 0] <- rgbColor["green",]/255
  alphaScale <- img[,,-1]
  img = array(0, dim=c(600,600,4))
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
sampleDFclustered = pdacData[sample(nrow(pdacData), 10000),]


#creating the plot
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
numUnique <-  length(unique(sampleDFclustered$location))
files <- c('checker', 'crisscross', 'polka', 'vertical', 'horizontal')
saveFileName <- paste0("D:/umd/summer 2020/patterns/", files, "Color.svg")
targetFiles <-  paste0("D:/umd/summer 2020/patterns/", files, "Circle.png")

for (i in 1:numUnique){
  CairoSVG(saveFileName[i], pointsize=1)
  plot(as.raster(colorChange(targetFiles[i], cbbPalette[i])))
  dev.off()
}

sampleImage = mapvalues(factor(sampleDFclustered$location), from=unique(sampleDFclustered$location), to=saveFileName[1:numUnique])
plt = ggplot(sampleDFclustered, aes(Xt, Yt)) + geom_image(image=sampleImage, size=0.2)
plot(plt)
