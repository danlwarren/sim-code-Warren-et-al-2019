# x is a moses.species object.  This function adds env data to training and
# test points

add.env <- function(x, env){
   
   # Check to make sure there's not already env data
   if(any(c(colnames(x$presence.points), colnames(x$background.points)) %in% (names(env)))){
      print("Object already has environmental data!")
   }
   
   # Add env data
   else{
      x$presence.points <- cbind(x$presence.points, extract(env, x$presence.points[,1:2]))
      x$background.points <- cbind(x$background.points, extract(env, x$background.points[,1:2]))
   }
   return(x)
}