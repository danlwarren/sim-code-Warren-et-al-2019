# Takes a moses.species object and a value of k, runs kfold on training and test data

moses.kfold <- function(x, k){
   
   # Check to make sure there's not already env data
   if(any("group" %in% c(colnames(x$presence.points), colnames(x$background.points)))){
      print("Object already has partitioning data!")
   }
   
   # Add env data
   else{
      x$presence.points$group <- kfold(x$presence.points, k)
      x$background.points$group <- kfold(x$background.points, k)
   }
   return(x)
}