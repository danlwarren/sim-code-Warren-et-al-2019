#' Defining a class for moses.species.  Each species gets: 
#' virtualspecies: a virtualspecies object which contains their niche and true suitability of habitat
#' actual.range: a raster with the actual range they occur in
#' presence.points: a data frame with sampled localities
#' background.points: a data frame with absence/pseudoabsence/background localities
#' species.name: a character vector with the species name 
#' models is going to be a list of models that are made for the species, which will
#'    be stuffed in there as we go along


moses.species <- function(virtualspecies = NA, actual.range = NA, presence.points = NA,
                          background.points = NA, species.name = NA, models=NA){
   
   # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
   # know how to do is.na on raster data, so it was barfing and error when a raster
   # was passed in.
   
   if(!isTRUE(is.na(virtualspecies))){
      if(!"virtualspecies" %in% class(virtualspecies)){
         print("Argument virtualspecies requires an object of class virtualspecies")
      }
   }
   
   if(!isTRUE(is.na(actual.range))){
      if(!any(c("RasterLayer", "RasterBrick") %in% class(actual.range))){
         print("Argument actual.range requires an object of class RasterLayer or RasterBrick")
      }
   }
   
   if(!isTRUE(is.na(presence.points))){
      if(!any("data.frame" %in% class(presence.points))){
         print("Argument presence.points requires an object of class data.frame")
      }
   }
   
   if(!isTRUE(is.na(background.points))){
      if(!any("data.frame" %in% class(background.points))){
         print("Argument background.points requires an object of class data.frame")
      }
   }
   
   if(!isTRUE(is.na(species.name))){
      if(!any("character" %in% class(species.name))){
         print("Argument species.name requires an object of class character")
      }
   }
   
   output <- list(
      virtualspecies = virtualspecies,
      actual.range = actual.range,
      presence.points = presence.points,
      background.points = background.points,
      models = models,
      species.name = species.name)
   
   class(output) <- c("list", "moses.species")
   
   return(output)
}

