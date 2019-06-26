#' This function will take the following arguments
#' root.species is a virtualspecies object for the root of the tree
#' ntaxa is the number of species to simulate
#' rate is the rate of evolution
#' env is a raster stack containing the environmental variables 
#' min.sd is the minimum proportion the sd can take compared to the root value, e.g., 0.5 means
#' that the minimum sd for a trait will be half that of the root value
#' ... is arguments to be passed to rTraitCont
#' 
#' 

evolve.clade <- function(root.species, ntaxa, env, ou=FALSE, rate = 0.2, min.sd = .5, ...){

   
   tip.names <- paste("species", 1:ntaxa, sep=".")
   
   # Make a random tree
   tree <- rtree(ntaxa, tip.label = tip.names)  
   
   # Get the names of the env vars, trim the stack to just those (virtualspecies requirement)
   env.vars <- root.species$details$variables
   env <- env[[env.vars]]
   
   plot(env)
   
   root.params <- root.species$details$parameters
   
   param.list <- list()
   
   plot(root.species$suitab.raster)
   
   # Creating a copy of root.params for each species, just to get the structure.
   # we'll modify these from the sims, and then we'll use them to build new species
   # from generateSpFromFun in virtualspecies
   for(i in tip.names){
      param.list[[i]] <- root.params
   }

   
   for(i in names(root.params)){
      # i is iterating over predictor variables
      for(j in names(root.params[[i]]$args)){
         
         if(grepl("sd", j)){
            # Brownian, but constraining the sd to be > a proportion of the root value
            this.root.value <- root.params[[i]][["args"]][j]
            
            
            # Not entirely clear why I have to make this.root.value numeric explicitly, but 
            # apparently I do
            this.trait <- rTraitCont(tree, 
                                     root.value = this.root.value, 
                                     theta = this.root.value, 
                                     sigma = rate * as.numeric(this.root.value), ...)
            
            for(k in tip.names){
               param.list[[k]][[i]][["args"]][j] <- max(abs(this.trait[k]), min.sd * this.root.value)
            }
         }
         else{
            
            this.root.value <- root.params[[i]][["args"]][j]
            
            
            # Not entirely clear why I have to make this.root.value numeric explicitly, but 
            # apparently I do
            this.trait <- rTraitCont(tree, 
                                     root.value = this.root.value, 
                                     theta = this.root.value, 
                                     sigma = rate * as.numeric(this.root.value), ...)
            
            for(k in tip.names){
               param.list[[k]][[i]][["args"]][j] <- this.trait[k]
            }
         }
      }
   }
   
   species <- list()
   for(i in names(param.list)){
      species[[i]] <- moses.species(virtualspecies =  generateSpFromFun(raster.stack = env,
                                                      parameters = param.list[[i]],
                                                      plot = TRUE),
                                    species.name = i)
   }
   
   root.species <- moses.species(virtualspecies = root.species, species.name = "root.species")
   
   
   output <- moses.clade(species = species, tree = tree, root.species = root.species)
}

