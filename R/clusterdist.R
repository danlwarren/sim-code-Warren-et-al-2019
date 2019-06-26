
# This function will take a pa raster from convertToPA and figure out a clustering scheme
# that maximimzes the minimum distance between clusters.  In other words, "given anywhere
# between 2 and N clusters, what number of clusters would be maximally allopatric?  Then
# it spits back a raster with an integer assigned to each species
clusterdist <- function(pa, maxclust, raster.template, plotty=FALSE){
  
  pa.table <- as.data.frame(rasterToPoints(pa$pa.raster))
  
  if(class(raster.template) == "RasterBrick"){
    raster.template = raster.template[[1]]
  }
  
  cluster.results <- c()
  for(i in 2:maxclust){
    # Splitting presence points into i clusters
    kmeans.out <- kmeans(pa.table[pa.table["layer"] == 1,1:2], iter.max = 100, nstart = 100, centers=i)
    
    # Assigning clusters to presence points
    pa.table[as.numeric(names(kmeans.out$cluster)),"cluster"] <- kmeans.out$cluster
    
    # Making a picture of the points (for bug hunting)
    if(plotty == TRUE){
      plot(raster.template)
      points(pa.table$y ~ pa.table$x, col=pa.table$cluster, main=paste(i, "clusters"))
    }
    
    # Setting mindist super high so it can decrease as we get values
    mindist <- 10000000
    for(i in 1:max(pa.table$cluster, na.rm=TRUE)){
      #get min rdist from cluster i to j, put it in mindist if it's lower than what's already there
      mindist <- min(mindist, min(rdist(pa.table[which(pa.table$cluster == i),1:2], pa.table[which(pa.table$cluster != i),1:2]), na.rm=TRUE))
    }
    cluster.results <- rbind(cluster.results, c(i, mindist))
    
  }
  if(plotty == TRUE){
    plot(cluster.results[,2] ~ cluster.results[,1], xlab=" Number of clusters", ylab="Minimum distance between clusters")
  }
  # Figuring out the maximum differential between successive cluster sizes - should correspond
  # to the point at which you start splitting natural clusters up.
  diffs <- cluster.results[1:nrow(cluster.results) - 1,2] - cluster.results[2:nrow(cluster.results),2]
  
  optimal.clusters <- cluster.results[which.max(diffs),1]
  
  print(paste("Returning solution with", optimal.clusters, "clusters."))
  
  # Splitting presence points into clusters
  kmeans.out <- kmeans(pa.table[pa.table["layer"] == 1,1:2], iter.max = 100, nstart = 100, centers=optimal.clusters)
  
  # Assigning clusters to presence points
  pa.table[as.numeric(names(kmeans.out$cluster)),"cluster"] <- kmeans.out$cluster
  
  # Making a picture of the points (for bug hunting)
  if(plotty == TRUE){
    plot(raster.template)
    points(pa.table$y ~ pa.table$x, col=pa.table$cluster, main=paste(optimal.clusters, "clusters"))
  }
  
  
  #print(head(pa.table))
  
  # Spitting out a raster
  pa.table <- pa.table[complete.cases(pa.table),]
  return(rasterize(pa.table[,1:2], raster.template, field=pa.table$cluster, fun='first'))
  
}

