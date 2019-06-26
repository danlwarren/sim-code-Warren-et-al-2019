# Takes a PA raster and a suitability raster, runs MigClim on it

moses.disperse <- function(pa.raster, suit.raster, ...){
   suit.points <- rasterToPoints(suit.raster)
   pa.points <- rasterToPoints(pa.raster)
   mig.df <- merge(pa.points, suit.points, by = c("x", "y"), all=TRUE)
   colnames(mig.df) <- c("lon", "lat", "pa", "suitability")
   
   mig.df[is.na(mig.df$pa),"pa"] <- 0
   mig.df$suitability <- (mig.df$suitability/max(mig.df$suitability)) * 1000
   
   n<-MigClim.migrate (iniDist=mig.df[,1:3], hsMap=mig.df[,4], rcThreshold=500, 
                       envChgSteps=1, dispSteps=5, dispKernel=c(1.0,0.4,0.16,0.06,0.03),
                       iniMatAge=1, propaguleProd=c(0.01,0.08,0.5,0.92),
                       lddFreq=0.1, lddMinDist=6, lddMaxDist=15, 
                       simulName="MigClimTest", replicateNb=1, overWrite=TRUE, 
                       testMode=FALSE, fullOutput=FALSE, keepTempFiles=FALSE)
   
   output <- list(mig.df = mig.df)
   
   return(output)
}


# writeRaster(test.new.allopatric.pa$pa.stack[["species.1"]], filename = "testpa.asc", format = "ascii", NAflag = -9999)
# 
# suit.raster <- my.clade$species$species.1$virtualspecies$suitab.raster/cellStats(my.clade$species$species.1$virtualspecies$suitab.raster, max) * 1000
# suit.raster <- setValues(suit.raster, as.integer(getValues(suit.raster)))
# writeRaster(suit.raster, 
#             filename = "testsuit1.asc", format = "ascii", NAflag = -9999)
# 
# n<-MigClim.migrate (iniDist="testpa",
#                     hsMap="testsuit", rcThreshold=0, 
#                     envChgSteps=1, dispSteps=5, dispKernel=c(1,0.4,0.16,0.06,0.03), 
#                     iniMatAge=1, propaguleProd=c(0.01,0.08,0.5,0.92), 
#                     lddFreq=0.1, lddMinDist=6, lddMaxDist=15, 
#                     simulName="MigClimTest", replicateNb=1, overWrite=TRUE, 
#                     testMode=FALSE, fullOutput=FALSE, keepTempFiles=FALSE)
# 
# results.raster <- raster("./MigClimTest/MigClimTest_raster.asc")
# plot(results.raster <= 4000)
# 
# test.disperse <- moses.disperse(test.new.allopatric.pa$pa.stack[["species.1"]],
#                  my.clade$species$species.1$virtualspecies$suitab.raster)


