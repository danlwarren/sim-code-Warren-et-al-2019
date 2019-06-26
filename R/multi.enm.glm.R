
multi.enm.glm <- function(species, env){
   
   # species is a list of moses.species objects
   # objects need to have p/a data, env data, and a grouping variable
   # env is our predictors
   
   # RETURN VALUES
   # data is the point data with environmental data added
   # each glm is returned in a separate list entry
   # pred.table has the AUC values for one group predicting the other
   # NOTE:  the model MAKING the prediciton is in the rows, the region being
   #        predicted ONTO is in the columns
   # truth.table contains I values for each model compared to the true suitability
   # of habitat
   
   merged.points <- merge.species.pa(species)
   group <- merged.points$group
   merged.points$species <- as.factor(merged.points$species)
   
   # Separate train and test data
   train.points <- merged.points[merged.points$group != 1,]
   test.points <- merged.points[merged.points$group == 1,]
   
   # Start building the output list
   output <- list(train.data = train.points, test.data = test.points)
   
   # Create lists for models and model predictions
   models <- list()
   predictions <- list()
   
   # Build glm with all species
   glm.all <- glm(pres ~ ., data=train.points[,c("pres", names(env))])
   
   # Stuff the all model into the models and predictions lists
   models[["all"]] <- glm.all
   predictions[["all"]] <- predict(env, glm.all)
   
   
   # Build a suitability raster for merged species by taking max suitability
   # in each grid cell
   suit.stack <- stack(lapply(species, function(x) x$virtualspecies$suitab.raster))
   merged.suitability <- calc(suit.stack, fun=max)
   
   # Start building the training prediction tables with all x all
   train.pred.table <- evaluate(train.points[train.points$pres == 1,], 
                                train.points[train.points$pres == 0,], glm.all)@auc
   test.pred.table <- evaluate(test.points[test.points$pres == 1,], 
                               test.points[test.points$pres == 0,], glm.all)@auc
   
   # Start building the truth table with all x truth
   truth.table <- raster.cor(predict(env, glm.all), merged.suitability)
   for(i in names(species)){
      truth.table <- c(truth.table, raster.cor(predict(env, glm.all), species[[i]]$virtualspecies$suitab.raster))
   }
   
   
   # Expand first row for glm.all predictions onto subsets
   for(i in levels(merged.points$species)){
      train.pred.table <- c(train.pred.table, 
                            evaluate(train.points[train.points$pres == 1 & train.points$species == i,], 
                                     train.points[train.points$pres == 0 & train.points$species == i,], 
                                     glm.all)@auc)
      test.pred.table <- c(test.pred.table, 
                           evaluate(test.points[test.points$pres == 1 & test.points$species == i,], 
                                    test.points[test.points$pres == 0 & test.points$species == i,], 
                                    glm.all)@auc)
   }
   
   # Now build and evaluate a model for each species separately
   for(i in levels(merged.points$species)){
      sp.train.points <- train.points[train.points$species == i,]
      sp.test.points <- test.points[test.points$species == i,]
      this.glm <- glm(pres ~ ., data=sp.train.points[,c("pres", names(env))])
      models[[i]] <- this.glm
      predictions[[i]] <- predict(env, this.glm)
      
      # Evaluate model starting with all species
      this.train.predline <- evaluate(train.points[train.points$pres == 1,], 
                                      train.points[train.points$pres == 0,], this.glm)@auc
      this.test.predline <- evaluate(test.points[test.points$pres == 1,], 
                                     test.points[test.points$pres == 0,], this.glm)@auc
      
      # Now with each individual species
      for(j in levels(train.points$species)){
         this.train.predline <- c(this.train.predline, 
                                  evaluate(train.points[train.points$pres == 1 & train.points$species == j,], 
                                           train.points[train.points$pres == 0 & train.points$species == j,], 
                                           this.glm)@auc)
      }
      for(j in levels(test.points$species)){
         this.test.predline <- c(this.test.predline, 
                                 evaluate(test.points[test.points$pres == 1 & test.points$species == j,], 
                                          test.points[test.points$pres == 0 & test.points$species == j,], 
                                          this.glm)@auc)
      }
      train.pred.table <- rbind(train.pred.table, this.train.predline)
      test.pred.table <- rbind(test.pred.table, this.test.predline)
      
      thisline.truth <- raster.cor(predict(env, this.glm), merged.suitability)
      
      for(i in names(species)){
         thisline.truth <- c(thisline.truth, 
                             raster.cor(predict(env, this.glm), (species[[i]]$virtualspecies$suitab.raster)))
      }
      
      truth.table <- rbind(truth.table, thisline.truth)
      
   }
   
   # Rename stuff to make it pretty
   rownames(train.pred.table) <- c("all", levels(merged.points$species))
   colnames(train.pred.table) <- c("all", levels(merged.points$species))
   rownames(test.pred.table) <- c("all", levels(merged.points$species))
   colnames(test.pred.table) <- c("all", levels(merged.points$species))
   rownames(truth.table) <- c("all", levels(merged.points$species))
   colnames(truth.table) <- c("all", levels(merged.points$species))
   truth.table <- data.frame(truth.table)
   
   # Chuck the final tables into the output
   output[["train.pred.table"]] <- train.pred.table
   output[["test.pred.table"]] <- test.pred.table
   output[["truth.table"]] <- truth.table
   output[["merged.suitability"]] <- merged.suitability
   output[["models"]] <- models
   output[["predictions"]] <- predictions
   output[["species"]] <- species
   
   class(output) <- c("list", "moses.multi.glm")
   
   return(output)
}
