#' Defining a class for moses.clade.  Each clade gets: 
#' species: a list of moses.species objects
#' tree: a tree showing the relationships between the species


moses.clade <- function(species = NA, tree = NA, root.species = NA){
   
   # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
   # know how to do is.na on raster data, so it was barfing and error when a raster
   # was passed in.
   
   if(!isTRUE(is.na(species))){
      
      # Checking to see if species is a list
      if(!"list" %in% class(species)){
         print("Argument species requires a list of moses.species objects")
      }
      
      # This if statement is asking whether any of the list elements don't have 
      # moses.species in their class definitions
      if(any(unlist(lapply(species, function(x) !"moses.species" %in% class(x))))){
         print("The following objects in the species list do not appear to be moses.species objects:")
         print(names(which(unlist(lapply(species, function(x) !"moses.species" %in% class(x))))))
      }
      
   }
   
   if(!isTRUE(is.na(tree))){
      # Checking to see if species is a list
      if(!"phylo" %in% class(tree)){
         print("Argument tree requires a phylo object")
      }
   }
   
   if(!isTRUE(is.na(root.species))){
      # Checking to see if species is a list
      if(!"moses.species" %in% class(root.species)){
         print("Argument root.species requires a moses.species object")
      }
   }
   
   output <- list(species = species,
                  tree = tree,
                  root.species = root.species)
   
   class(output) <- c("list", "moses.clade")
   
   return(output)
}

