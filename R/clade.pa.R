# takes a moses.clade obect, returns a stack of PA rasters, a table with presences
# for all species, and a revised clade with presence data.  Bias raster is multiplied by
# suitability to get sample probability, and if npres isn't NA only that many presences
# will be returned..

clade.pa <- function(x, sample.source = "suitab.raster", bias.raster = NA, 
                     npres = NA, method = "auto", ...){
   
   output.stack <- stack(x$species[[1]]$virtualspecies$suitab.raster)
   
   pa.table <- data.frame("x" = numeric(0), "y" = numeric(0), "species" = character(0))
   
   if(is.na(bias.raster)){
      bias.raster <- x$species[[1]]$virtualspecies$suitab.raster
      bias.raster[!is.na(bias.raster)] <- 1
   }
   
   clade = x
   
   for(i in x$species){
      this.npres <- 0
      print(i$species.name)
      sample.raster <- c()
      #plot(i$virtualspecies$suitab.raster)
      if(sample.source == "suitab.raster"){
         sample.raster <- i$virtualspecies$suitab.raster * bias.raster
      } else if(sample.source == "actual.range"){
         sample.raster <- i$actual.range * bias.raster
      }
      plot(sample.raster)
      if(method == "auto"){
         # Auto beta
         rastervals <- getValues(sample.raster)
         rastervals <- rastervals[!is.na(rastervals)]
         #print(length(rastervals))
         this.beta <- 0
         if(is.na(npres)){
            this.npres <- length(rastervals)
         }
         else{
            this.npres <- npres
         }
         if(length(rastervals) <= this.npres * 4){
            # No way to get more than 4 x npres points
           print(1)
            pa <- convertToPA(sample.raster, PA.method = "threshold", beta = min(rastervals), ...)    
         }
         else{
            # Picks a beta such that at least npres values exceed beta
           print(2)
            this.beta <- sort(rastervals)[length(rastervals) - this.npres * 4]
            pa <- convertToPA(sample.raster, PA.method = "threshold", beta = this.beta, ...)
         }
         
      }
      
      else{ # Method is not "auto"
         rastervals <- getValues(sample.raster)
         rastervals <- rastervals[!is.na(rastervals)]
         if(is.na(npres)){
            this.npres <- length(rastervals)
         }
         else{
            this.npres <- npres
         }
         # Not auto beta
         print(3)
         print(method)
         pa <- convertToPA(sample.raster, PA.method=method,  ...)    
      }
      
      #print(pa$pa.raster)
      
      output.stack[[i$species.name]] <- pa$pa.raster
      
      this.pa <- data.frame(rasterToPoints(pa$pa.raster))
      this.pres <- this.pa[this.pa[,3] == 1,1:2]
      
      prob <- extract(sample.raster, this.pres)

      this.pres <- this.pres[sample(1:nrow(this.pres), prob = prob, size=min(this.npres, nrow(this.pres))),]
      
      this.pres$species <- rep(i$species.name, nrow(this.pres))
      
      
      pa.table <- rbind(pa.table, this.pres)
      
      this.pres$pres <- rep(1, nrow(this.pres))
      colnames(this.pres) <- c("lon", "lat", "species", "pres")
#       if(!is.na(npres)){
#          this.pres <- this.pres[sample(1:nrow(this.pres), npres),]
#       }
      clade$species[[i$species.name]]$presence.points <- this.pres
      
      this.abs <- this.pa[this.pa[,3] == 1,1:2]
      this.abs$species <- rep(i$species.name, nrow(this.abs))
      
      this.abs$pres <- rep(0, nrow(this.abs))
      colnames(this.abs) <- c("lon", "lat", "species", "pres")
      clade$species[[i$species.name]]$background.points <- this.abs
      
   }
   
   colnames(pa.table) <- c("lon", "lat", "species")
   
   output.stack <- dropLayer(output.stack, 1)
   
   output <- list(pa.rasters = output.stack,
                  pa.table = pa.table, clade = clade)
   
   return(output)
}