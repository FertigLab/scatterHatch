context("Testing erroneous/bad input handling")

# setting up pdac data set
pdacData <- pdacData
pdacData$cellID <- paste0('cell_', 1:nrow(pdacData))
pdacData$Yt <- -pdacData$Yt
pancreas_frames <- c(1:6, 27:31, 15:19, 40:44)
PDAC_frames <- c(23:26, 35:37, 51:52, 64:65, 77)
small_intestines_frames <- c(49:50, 63, 75:76, 88:89, 100:103, 112:116, 125:129, 137:140)
annotateLocation <- function(frame){
  if (frame %in% pancreas_frames){return("Pancreas")}
  if (frame %in% PDAC_frames){return("PDAC")}
  if (frame %in% small_intestines_frames){return("Small Intestine")}
  return("Other")
}
pdacData$location <- sapply(pdacData$frame, annotateLocation)



test_that("incorrect x column name", {
  expect_error(scatterHatch(pdacData, "", "Yt", "location"),
               "x column name not present in dataset.")
})

test_that("character or complex type x column", {
  pdacData$botchedX <- rep("bad", nrow(pdacData))
  expect_error(scatterHatch(pdacData, "botchedX", "Yt", "location"),
               "x column is not numeric.")

  pdacData$botchedX <- rep(complex(real=1, imaginary=1), nrow(pdacData))
  expect_error(scatterHatch(pdacData, "botchedX", "Yt", "location"),
               "x column is not numeric.")
})

test_that("incorrect y column name", {
  expect_error(scatterHatch(pdacData, "Xt", "", "location"),
               "y column name not present in dataset.")
})

test_that("character or complex type y column", {
  pdacData$botchedY <- rep("bad", nrow(pdacData))
  expect_error(scatterHatch(pdacData, "Xt", "botchedY", "location"),
               "y column is not numeric.")

  pdacData$botchedY <- rep(complex(real=1, imaginary=1), nrow(pdacData))
  expect_error(scatterHatch(pdacData, "Xt", "botchedY", "location"),
               "y column is not numeric.")
})

test_that("incorrect factor name", {
  expect_error(scatterHatch(pdacData, "Xt", "Yt", ""),
               "factor column name not present in dataset.")
})

test_that("inputted patternList less than number of groups", {
  patternList <- list(list(pattern = "/"), list(pattern = "|"))
  expect_error(scatterHatch(pdacData, "Xt", "Yt", "location", patternList = patternList),
               "The length of patternList must be greater than or equal to the number of groups present.")
})

test_that("# of unique pattern/color combinations less than # of groups", {
  colorPalette <- c("#FF0000", "#00FF00")
  expect_error(scatterHatch(pdacData, "Xt", "Yt", "frame", colorPalette = colorPalette),
               "Not enough unique combinations of patterns and columns for each group.")

  patternList <- rep(list(list(pattern = "/"), list(pattern = "-")), 80)
  expect_error(scatterHatch(pdacData, "Xt", "Yt", "frame", patternList = patternList),
               "Not enough unique combinations of patterns and columns for each group.")

})

test_that("repeating colors with different patterns", {
  colorPalette <- c("#FF0000", "#00FF00")
  expect_warning(scatterHatch(pdacData, "Xt", "Yt", "location", colorPalette = colorPalette),
                 "Same point colors will be repeated with different hatching patterns!")

})
