library(raster)
library(rgdal)


#Takes one raster layer, changes all NA values in the cells to 0
resetNAvalues <-  function(raster) {
  nraster <- reclassify(raster, c(NA,0))
  return(nraster)
}

#Takes one raster layer, and returns a raster layer of the focal sum for each cell
#Uses window size of n x n
focalSumRast <- function(raster, n) {
  nraster <- focal(raster, w=matrix(1, nrow=n, ncol=n))
  return(nraster)
}

#Takes a layer of focal sums and returns a binary layer where only cells on the border are 1
rastBorderCrop <- function(raster, n) {
  
  nraster <- reclassify(raster, c(0, n-1, 0,
                                  n-1, n+2,   1,
                                  n+2, Inf, 0))
  return(nraster)
}

#Takes one raster layer and returns a binary layer
#Where cells whose value is n is 1
rastToBinary <- function(raster, n) {
  m <-  c(0, n, 0,
          n, n, 1,
          n, Inf, 0)
  rcl <- matrix(m, ncol=3, byrow=TRUE)
  nraster <- reclassify(raster, rcl)
  return(nraster)
}

#Takes one binary layer and returns a list of two vectors
#Containing the coordinates to all cells where the value is 1
#Too slow, do not use
binaryToSPDF <- function(raster) {
  xvec = c()
  yvec = c()
  
  x <- 1
  y <- 1
  
  for (x in seq(raster@nrows)) {
    for(y in seq(raster@ncols)){
      if (raster[x,y] == 0 | is.na(raster[x,y])) {
        next
      } else {
        testpoint <- xyFromCell(water, x, y, spatial = TRUE)
        xvec <- c(xvec, testpoint@coords[1])
        yvec <- c(yvec, testpoint@coords[2])
      }
    }
  }
  
  veclist <- list(xvec, yvec)
  return(veclist)
}

#Takes one classified layer (integer values) and one binary layer
#Changes all cells in the classified layer, where the corresponding binary cell is 1
#To the same value, n
classOverlay <- function(classrast, binrast, n) {
  newclassed <- projectRaster(classrast, binrast, method = "ngb")
  newraster <- overlay(binrast, newclassed, fun=function(x,y){return(100*x+y)})
  m <- c(0, 1, 0,
         1, 2, 2,
         2, 3, 3,
         3, 4, 4,
         4, 5, 5,
         5, 6, 6,
         6, 7, 7,
         7, 8, 8,
         8, 9, 9,
         9, Inf, n)
  rcl <- matrix(m, ncol=3, byrow=TRUE)
  newraster <- reclassify(newraster, rcl)
  return(newraster)
}

###################################################################
##########################DEMO THINGS##############################
###################################################################

#Data import and adjustments
water <- raster("justwater.tif")
resetNAvalues(water)
plot(water)

classed <- raster("classified.tif")
plot(classed)

#Creating a raster border of water
water.focalsum <- focalSumRast(water, 3)
plot(water.focalsum)

waterborder <- rastBorderCrop(water.focalsum, 4)
plot(waterborder)

#Creating a raster border using classed
reclassed <- rastToBinary(classed, 1)
reclassed <- focalSumRast(reclassed, 3)
reclassed <- rastBorderCrop(reclassed, 4)
plot(reclassed)

#Class overlay
#newrast <- classOverlay(classed, water, 1)

#This takes too long for the demo:
#newclassed <- projectRaster(classed, water, method = "ngb")
#writeRaster(newclassed, "newclassed.tif", format = "GTiff", overwrite = TRUE)
#Instead import an already done raster
newclassed <- raster("newclassed.tif")

newraster <- overlay(water, newclassed, fun=function(x,y){return(10*x+y)})
m <- c(0, 1, 0,
       1, 2, 2,
       2, 3, 3,
       3, 4, 4,
       4, 5, 5,
       5, 6, 6,
       6, 7, 7,
       7, 8, 8,
       8, 9, 9,
       9, Inf, 1)
rcl <- matrix(m, ncol=3, byrow=TRUE)
newraster <- reclassify(newraster, rcl)
plot(newraster)
