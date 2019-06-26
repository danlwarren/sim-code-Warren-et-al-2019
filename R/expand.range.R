# Takes a p/a raster, suitability raster, and buffer width.  Returns a raster that has 
# 1 everywhere that's present in PA raster, has the suitability value everywhere that's NOT present
# but within buffer width, and zero everywhere that's outside buffer width

expand.range <- function(suit.raster, pa.raster, width = 1){
   buffer.shape <- gBuffer(rasterToPolygons(pa.raster, dissolve=TRUE, fun=function(x){x>0}), width=width)
   buffer.raster <- rasterize(buffer.shape, suit.raster, 1)
#    expanded.range <- max((suit.raster * buffer.raster), pa.raster)
   expanded.range <- (suit.raster * buffer.raster)
   # plot(expanded.range)
   return(expanded.range)
}

