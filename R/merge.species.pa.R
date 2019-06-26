# Takes a list of moses.species objects, binds together all of their presence
# and background tables, returns a df

merge.species.pa <- function(species.list){
   presence <- do.call(rbind, lapply(species.list, function(x) x$presence.points))
   background <- do.call(rbind, lapply(species.list, function(x) x$background.points))
   return(rbind(presence, background))
}