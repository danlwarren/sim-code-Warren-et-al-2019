# Takes a raster and sets minimum value to zero

raster.limit <- function(x){
   return(x - cellStats(x, min))
}