

# Does overlap for a stack of rasters

stack.overlap <- function(stack, ...){
   output <- matrix(NA, nrow = length(names(stack)), ncol=length(names(stack)))
   colnames(output) <- names(stack)
   rownames(output) <- names(stack)
   
   for(i in names(stack)){
      for(j in names(stack)){
         output[i,j] <-nicheOverlap(raster.limit(stack[[i]]), raster.limit(stack[[j]]), ...)  
      }
   }
   
   return(data.frame(output))
}