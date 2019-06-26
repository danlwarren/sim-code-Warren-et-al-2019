# Returns a correlation coefficient for values in two rasters

raster.cor <- function(x, y, method="spearman", g = 10){
  x <- getValues(x)
  y <- getValues(y)
  data <- cbind(x, y)
  data<- data[complete.cases(data),]
  if(method == "hoslem"){
    # Modified from http://sas-and-r.blogspot.com.au/2010/09/example-87-hosmer-and-lemeshow-goodness.html
    cutyhat <- cut(data[,2],
                   breaks = quantile(data[,2], probs=seq(0,
                                                         1, 1/g)), include.lowest=TRUE)
    obs <- xtabs(cbind(1 - data[,1], data[,1]) ~ cutyhat)
    expect <- xtabs(cbind(1 - data[,2], data[,2]) ~ cutyhat)
    chisq <- sum((obs - expect)^2/expect)
    P <- 1 - pchisq(chisq, g - 2)
    return(chisq)
  } else {
    return(cor(data[,1], data[,2], method=method))
  }
}