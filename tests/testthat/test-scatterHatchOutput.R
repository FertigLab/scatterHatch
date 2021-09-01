context("Testing scatterHatch output")

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


test_that("Plot layers match expectations", {
  plt = scatterHatch(data = pdacData, x = "Xt", y = "Yt", color_by = "location")
  expect_equal(length(plt$layers), 6)
})

test_that("Output can plot", {
  plt = scatterHatch(data = pdacData, x = "Xt", y = "Yt", color_by = "location")
  expect_error(print(plt), NA)
})
