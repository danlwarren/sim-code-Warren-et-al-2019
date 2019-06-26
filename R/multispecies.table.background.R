# Takes occurrence points for a set of species, samples background
# data from circular buffers for each species with a given prevalence

multispecies.table.background <- function(points, env, buffer.dist, prevalence){
   # Assign our incoming data to be presences
   points$pres <- rep(1, nrow(points))
   
   # Now get absences for all species
   for(i in levels(points$species)){
      thesepoints <- points[points$species == i,]
      npoints <- as.integer(((1/prevalence) - 1) * nrow(thesepoints))
      this.bg <- data.frame(background.buffer(thesepoints, buffer.dist, npoints, env))
      this.bg$pres <- rep(0, nrow(this.bg))
      this.bg$species <- rep(i, nrow(this.bg))
      points <- rbind(points, this.bg)
   }
   
   return(points)
}