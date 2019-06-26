# Takes a virtualspecies object and unlists all of the args in the niche parameters.  
# If you don't do this after generating random species, ape will just absolutely shit
# itself and crash R entirely.

unlist.args <- function (x){
   for(i in x$details$variables){
      x$details$parameters[[i]]$args <- unlist(x$details$parameters[[i]]$args)
   }
   return(x)
}