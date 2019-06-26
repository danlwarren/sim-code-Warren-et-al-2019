# Takes a phy object, spits back a list of clade members for every internal node

get.clades <- function(tree, ...){
   output <- list()
   cladecounter <- 1
   
   for(i in 1:(length(tree$edge)/2 + 1)){
      desc <- tree$edge[tree$edge[,1] == i,2]
      if(length(desc) > 0){
         for(j in 1:length(desc)){
            if(length(tips(tree, desc[j])) > 1){
               output <- c(output, list(tips(tree, desc[j])))
            }
         }
      }
   }
   return(output)
}

