# Rotates the coordinate system
# Tejas Guha - January 1, 2021

#' Finds the equivalent cartesian coordinates for points in a rotated coordinate system
#'
#' @param x Vector of the x coordinates
#' @param y Vector of the y coordinates
#' @param angle Angle to rotate the coordinate system by (in degrees)
#' @return Dataframe with 2 columns specifying the transformed coordinates for each point
#' @export
rotateCoords <- function(x, y, angle){
  radians <- (angle/180) * pi
  xRotate <- (x * cos(radians)) - (y * sin(radians))
  yRotate <- (x * sin(radians)) + (y * cos(radians))
  return(data.frame(x = xRotate, y = yRotate))
}
