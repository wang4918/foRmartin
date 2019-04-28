library(raster)
library(rgdal)
water <- raster("justwater.tif")
water
water[3654,5312]
water[]

resetNAvalues <-  function(raster) {
  nraster <- reclassify(raster, c(NA,0))
  nraster
}

resetNAvalues(water)
plot(water)

focalSumRast <- function(raster, n) {
  nraster <- focal(raster, w=matrix(1, nrow=n, ncol=n))
  nraster
}

water.focalsum <- focalSumRast(water, 3)
plot(water.focalsum)

rastBorderCrop <- function(raster, n) {
  
  nraster <- reclassify(raster, c(0, n-1, 0,
                                  n-1, n+2,   1,
                                  n+2, Inf, 0))
}

waterborder <- rastBorderCrop(water.focalsum, 4)
plot(waterborder)
writeRaster(waterborder, "waterborder.tif", format = "GTiff", overwrite = TRUE)


classed <- raster("classified.tif")
plot(classed)

writeRaster(water.focalsum, "waterfocal.tif", format = "GTiff")

testpoint <- xyFromCell(water, 3654,5312, spatial = TRUE)
testpoint2 <- xyFromCell(water, 3656,5378, spatial = TRUE)

testpoint@coords[2]

plot(S)

xvec = c()
yvec = c()

x <- 1
y <- 1

for (x in seq(waterborder@nrows)) {
  for(y in seq(waterborder@ncols)){
    if (waterborder[x,y] == 0 | is.na(waterborder[x,y])) {
      next
    } else {
      testpoint <- xyFromCell(water, x, y, spatial = TRUE)
      xvec <- c(xvec, testpoint@coords[1])
      yvec <- c(yvec, testpoint@coords[2])
    }
  }
}

addRaster <- function(x,y){
  x + y
}

plot(classed)
newclassed <- classed
newwater <- water

#newclassed <- crop(newclassed, water)
newwater <- crop(water, newclassed)
plot(newclassed)

newclassed <- projectRaster(classed, water, method = "ngb")

watercontour <- rasterToContour(water, 7831*7961)
plot(watercontour)

newclassed@extent <- alignExtent(water@extent, newclassed, snap="near")
newraster <- overlay(water, newclassed, fun=function(x,y){return(10*x+y)})
plot(newraster)

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
