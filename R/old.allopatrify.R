# Takes a df, the name of the species column, and the columns of variables to split on (lon/lat and/or environmental)
# does a qda to break apart populations, and returns presence/absence for each species
# The threshold variable sets the minimum posterior probability needed under the qda for a species
# to be counted present.  Higher thresholds = more allopatry.

allopatrify <- function(x, species.var, split.cols, raster.template, threshold = 0.1){
   
   # Start building the output df
   pa.table <- data.frame(x)
   
   # Set up a raster stack
   raster.template[!is.na(raster.template)] <- 0
   pa.stack <- stack(raster.template)
   
   # Set up a formula using the supplied columns
   split.formula <- as.formula(paste(species.var, " ~ ", paste(split.cols, collapse= "+")))
   
   # Do quadratic discriminant analysis to calculate posteriors for presence
   this.qda <- qda(split.formula, data=x, CV = TRUE)
   
   sp.assigned = c()
   
   # Threshold posteriors to get actual presence
   if(threshold == "max"){
      
      # This one is causing a lot of threshold errors because some species aren't max posterior
      # in any grid cell.  Could be ties?
      
      scaled.posterior <- apply(this.qda$posterior, 2, function(x) x/sum(x))
      print(head(scaled.posterior))
      
      # Not using which.max because it doesn't return ties
      sp.assigned <- apply(scaled.posterior, 1, function(x) names(which(x == max(x))))
   }
   else{
      sp.assigned <- apply(this.qda$posterior, 1, function(x) names(which(x > threshold)))   
   }
   
   
   # Build a PA table and rasters
   for(i in levels(as.factor(x[,species.var]))){
      pa.table[,i] <- unlist(lapply(sp.assigned, function(x) as.numeric(as.character(i) %in% x)))
      
      if(sum(pa.table[,i]) == 0){
         stop(paste("Threshold too high,", i, "has nowhere to live!"), call. = FALSE)
      }
      
      table.for.raster <- pa.table[pa.table[,i] == 1,c(1,2)]
      
      pa.stack[[i]] <-  rasterize(table.for.raster, raster.template, field=1)
   }
   
   # Drop the template layer from the stack
   pa.stack <- dropLayer(pa.stack, 1)
   
   plot(pa.stack)
   
   output <- list(pa.table = pa.table, pa.stack = pa.stack, qda = this.qda)
   
   return(output)
}


# NOTE: If you're getting an error that says "Error in aggregate.data.frame(field, list(cells), fun, na.rm = na.rm) : 
# no rows to aggregate", the threshold is too high. 
