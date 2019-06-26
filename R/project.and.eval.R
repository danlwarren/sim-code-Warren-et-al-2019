# This function takes the output of a moses multi glm object and a set of environments.
# It projects all of hte models and true niches to the set of environmental variables
# and returns overlap scores for the resulting rasters.
#
# The model name is in the row, and the true species suitabiilty is in the column


project.and.eval <- function(x, env){
   
   prediction.table <- data.frame(matrix(NA, nrow = length(x$models), ncol = length(x$species)))
   
   # We're going to step through each model/species pair
   
   predicted <- lapply(x$models, function(this.model) predict(env, this.model))
   
   species <- x$species
   true <- lapply(species, function(this.species) generateSpFromFun(env[[this.species$virtualspecies$details$variables]], 
                                                                      this.species$virtualspecies$details$parameters)$suitab.raster)
   
   for(i in 1:length(names(predicted))){
      for(j in 1:length(names(true))){
         prediction.table[i,j] <- raster.cor(predicted[[i]], true[[j]])
      }
   }
   
   rownames(prediction.table) <- names(x$models)
   colnames(prediction.table) <- names(x$species)
   
   # Here we're getting a vector of how well the "all" model predicts each species
   all.preds <- prediction.table["all",]
   
   # And here we're getting a vector for each species predicting itself
   self.preds <- c()
   for(i in names(x$models)){
      self.preds <- c(self.preds, prediction.table[i,i])
   }
   
   # Making a df for qplot of all vs self
   plot.df <- data.frame(cor = unlist(c(self.preds, all.preds)))
   plot.df$predictor <- c(rep("self", length(self.preds)), rep("all", length(all.preds)))
   self.vs.all <- qplot(cor, data=plot.df, fill=predictor, alpha=0.5)
   
   return(list(predicted = predicted, 
               true = true, 
               prediction.table = prediction.table,
               self.vs.all = self.vs.all,
               plot.df = plot.df))
}