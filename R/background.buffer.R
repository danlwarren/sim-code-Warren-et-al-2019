# Takes a set of points, a buffer radius, a sample size, and a mask and returns 
# randomly sampled points from within that buffer radius.  Code modified from 
# Elith and Hijmans SDM with R tutorial

background.buffer <- function(points, radius, n, mask){
  x <- circles(points, d=radius, lonlat=TRUE)
  pol <-  gUnaryUnion(x@polygons)
  buffer.raster <- mask(mask, pol)
  xy <- sampleRandom(buffer.raster, size=n, xy=TRUE)
  colnames(xy)[1:2] <- c("lon", "lat")
  return(xy[,1:2])
}