# Function for printing moses.species objects

print.moses.species <- function(x, print.tables = TRUE, ...){
   
   
   # Printing bits of the object for summary purposes
   plot.me <- FALSE
   
   if(!isTRUE(is.na(x$species.name))){
      print(paste("Species.name:", x$species.name ))
   } 
   
   if(!isTRUE(is.na(x$virtualspecies))){
      plot(x$virtualspecies)
      plot.me <- TRUE
   }
   
   if(!isTRUE(is.na(x$actual.range))){
      plot(x$actual.range, add=TRUE, alpha=0.8, 
           col=colorRampPalette(c("blue", "yellow", "red"))(100),
           legend=FALSE)
      plot.me <- TRUE
   }
   
   if(!isTRUE(is.na(x$background.points))){
      if(plot.me == TRUE){
         points(x$background.points, pch="-", col="red")
      }
      if(print.tables == TRUE){
         print("Background points:")
         print(head(x$background.points))   
      }
   }
   
   if(!isTRUE(is.na(x$presence.points))){
      if(plot.me == TRUE){
         points(x$presence.points, col="black", pch=16)
      }
      if(print.tables == TRUE){
         print("Presence points:")
         print(head(x$presence.points))   
      }
   }
   
   if(!isTRUE(is.na(x$models))){
      print(paste("Models:", x$models ))
   } 
}