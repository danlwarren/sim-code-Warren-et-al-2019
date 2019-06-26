# Takes a moses.clade object, env raster stack, buffer dist and prevalence
# Returns a clade with background data samples from buffers around presence points

clade.background <- function(x, env, buffer.dist, prevalence){
   clade = x
   
   for(i in x$species){
      pres.points <- i$presence.points
      plot(rasterize(pres.points[,1:2], env[[1]]))
      npoints <- as.integer(((1/prevalence) - 1) * nrow(pres.points))
      this.bg <- data.frame(background.buffer(pres.points, buffer.dist, npoints, env))
      this.bg$species <- rep(i$species.name, nrow(this.bg))
      this.bg$pres <- rep(0, nrow(this.bg))
      colnames(this.bg) <- c("lon", "lat", "species", "pres")
      clade$species[[i$species.name]]$background.points <- this.bg
   }
   
   return(clade)
}