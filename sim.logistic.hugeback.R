library(ggplot2)
source("mod.randomPoints.R")
library(rmarkdown)
library(dismo)
library(randomForest)
library(mgcv)

sim.logistic <- function(nvars = 3, bias.raster, bias.strength = 1, npoints = 100, outfile, 
                                  n.background = 1000, buffer.width = 1000000, allopatry = allopatry, ...){
  outhtml <- paste0(outfile, ".html")
  knit2html('./sim.logistic.hugeback.Rmd', outfile, force_v1 = TRUE)
}


