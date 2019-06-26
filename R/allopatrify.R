#  Takes a moses.species object, a raster template, and a buffer width.
#  Breaks combined range apart into N clusters using kmeans clustering, and then
#  assigns each cluster to the species whose PA centroid is closest to that cluster.
#  Then it buffers the intersection of the cluster and the species PA out by buffer.width,
#  and multiplies by the suitability raster.  That gives us the suitability of habitat within
#  the actual range of the species.  Everything is then stuffed into a moses.clade object.

allopatrify <- function(x, raster.template, buffer.width = 1, plot=TRUE){
   
   # These two were originally supplied as arguments.  May want to change back to that later.
   species.var <- "species"
   split.cols <- c("lon", "lat")
   
   print("Getting pa data...")
   
   pa <-  clade.pa(x, sample.source = "suitab.raster", method = "probability", beta = 0.5, alpha = -0.00007, plot = plot)
   
   # Start building the output df
   pa.table <-  data.frame(pa$pa.table)
   
   # Set up raster stacks
   raster.template[!is.na(raster.template)] <- 0
   cluster.stack <- stack(raster.template)
   suit.stack <- stack(raster.template)
   pa.stack <- stack(raster.template)
   sample.stack <- stack(raster.template)
   
   # Get count of presence cells for each species, sorting smallest to largest
   breadth <- sort(table(pa.table[species.var]))
   
   # Get centroids of suitable habitat
   #print(head(x.pa))
   centroids <- ddply(pa.table[,c(species.var, split.cols)], species.var, numcolwise(mean))
   
   # Get kmeans clusters
   unique.rows <- unique(pa.table[,split.cols])
   clusters <- kmeans(unique.rows, length(unique(pa.table[,species.var])), nstart=1, iter.max=1000)
   unique.clusters <- cbind(unique(unique.rows[c("lon", "lat")]), paste("cluster", clusters$cluster, sep="."))
   colnames(unique.clusters) <- c("lon", "lat", "cluster")
   
   # This is just for a quick prototype, might want to make it more general eventually
   cluster.latlon <- cbind(unique(unique.rows[c("lon", "lat")]), clusters$cluster)
   colnames(cluster.latlon) <- c("lon", "lat", "cluster")
   cluster.raster <- rasterize(cluster.latlon[,c("lon", "lat")], raster.template, field=cluster.latlon[,"cluster"])
   plot(cluster.raster)
   
   
   # Making a unique raster for each cluster, putting them in a stack
   for(i in unique(cluster.latlon[,"cluster"])){
      cluster.stack <- addLayer(cluster.stack, cluster.raster == i)
   }
   cluster.stack <- dropLayer(cluster.stack, 1)
   names(cluster.stack) <- unique(cluster.latlon[,"cluster"])
   
   # Making a raster of suitable habitat for each species, putting it in a stack
   for(i in unique(pa.table[,species.var])){
      suit.stack <- addLayer(suit.stack, rasterize(pa.table[pa.table[species.var] == i,c("lon", "lat")], suit.stack), field=1)
   }
   suit.stack <- dropLayer(suit.stack, 1)
   names(suit.stack) <- unique(pa.table[,species.var])
   
   print("allopatrifying...")
   
   # Stepping through from narrowest range to largest, putting species where they overlap with their cluster
   temp.cluster <- cluster.stack
   for(i in names(breadth)){
      #print(i)
      this.sp <- suit.stack[[i]]
      overlaps <- c()
      for(j in names(temp.cluster)){
         overlaps <- c(overlaps, cellStats(this.sp * temp.cluster[[j]], stat="sum"))
      }
      this.pa <- this.sp * temp.cluster[[which.max(overlaps)]]
      
      # Some species don't overlap with any remaining clusters, they get their original range back
      if(cellStats(this.pa, sum) == 0){
         this.pa <- this.sp
      }
      pa.stack[[i]] <- this.pa
      
      # Dropping out layers that have already been assigned
      temp.cluster <- dropLayer(temp.cluster, which.max(overlaps))
   }
   pa.stack <- dropLayer(pa.stack, 1)
   
   # Convert to 1/0
   pa.stack <- pa.stack > 0
   
   if(plot == TRUE){
      plot(pa.stack)   
   }
   
   print("sympatrifying...")
   
   for(i in names(breadth)){
      #print(i)
      this.range <- expand.range(x$species[[i]]$virtualspecies$suitab.raster,
                                 pa.stack[[i]], buffer.width)
      sample.stack[[i]] <- this.range
   }
   sample.stack <- dropLayer(sample.stack, 1)
   
   if(plot == TRUE){
      plot(sample.stack)   
   }
   
   # Creating a moses.clade object for output
   output <- x
   
   for(i in names(sample.stack)){
      output$species[[i]]$actual.range <- sample.stack[[i]]
   }
   
   return(output)
}


