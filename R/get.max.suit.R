# Expets a list of moses.species objects, returns the xy represending max suitability for each species

get.max.suit <- function(species){
   output <- lapply(species, function(x) xyFromCell(x$virtualspecies$suitab.raster, 
                                    which.max(x$virtualspecies$suitab.raster)))
   return(output)
}
