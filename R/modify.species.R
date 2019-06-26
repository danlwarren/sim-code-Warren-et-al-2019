#' This function will take the following arguments
#' start.species is a virtualspecies object to start from
#' delta is the proportional change in each variable from the start species
#' env is a raster stack containing the environmental variables 
#' 
#' 

modify.species <- function(start.species, env, delta = 0){
   
   # Get the names of the env vars, trim the stack to just those (virtualspecies requirement)
   env.vars <- start.species$details$variables
   env <- env[[env.vars]]
   
   # plot(env)
   
   new.params <- start.species$details$parameters
   
   for(i in names(new.params)){
     new.params[[i]][["args"]][names(new.params[[i]][["args"]]) != "sd"] <- 
        new.params[[i]][["args"]][names(new.params[[i]][["args"]]) != "sd"] + 
        new.params[[i]][["args"]][names(new.params[[i]][["args"]]) != "sd"] * delta
   }
   
   new.species <- generateSpFromFun(raster.stack = env,
                                    parameters = new.params,
                                    plot = TRUE)
   # 
   # species <- list()
   # for(i in names(param.list)){
   #    species[[i]] <- moses.species(virtualspecies =  generateSpFromFun(raster.stack = env,
   #                                                                      parameters = param.list[[i]],
   #                                                                      plot = TRUE),
   #                                  species.name = i)
   # }
   # 
   # start.species <- moses.species(virtualspecies = start.species, species.name = "start.species")
   # 
   # 
   # output <- moses.clade(species = species, tree = tree, start.species = start.species)
   
   return(new.species)
}

