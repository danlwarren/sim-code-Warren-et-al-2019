# x is the environment matrix
#
# cols is a list of columns to vicariate on (I hope that's a word, but if not it is now).  If
# passed XY, it does clustering based on that.  If passed environmental variables, it does
# clustering based on that.
#
# pa is the presence/absence of the ancestor
#
# erode.prop is the proportion of points to erase from the space between species

vicariate <- function(x, cols, pa, erode.prop=0)
{
    # Doing K means.  kmeans.results is a table of indices and whether they belong to sp 1 or 2
    kmeans.results <- cbind(x[which(pa==1),1] ,kmeans(x[which(pa==1),cols], centers=2)$cluster)
    
    # Setting up empty pa columns with NA in the right places and 0 everywhere else
    sp1.pa <- 0 * pa
    sp2.pa <- 0 * pa   
    
    # Setting cells for species to "present" using the indices from clustering
    sp1.ind <- kmeans.results[which(kmeans.results[,2] == 1),1] 
    sp2.ind <- kmeans.results[which(kmeans.results[,2] == 2),1] 
    
    sp1.pa[sp1.ind] <- 1
    sp2.pa[sp2.ind] <- 1
    
    kmeans.results <-cbind(sp1.pa, sp2.pa)
    if(erode.prop > 0)
    {
        kmeans.results <- erode(x, cols, kmeans.results, erode.prop)
    }
    
    return(kmeans.results)
}