library(ggplot2)
source("mod.randomPoints.R")
library(rmarkdown)
library(dismo)
library(randomForest)
library(mgcv)
library(fields)
library(rgeos)
library(ENMeval)
library(ResourceSelection)

sim.logistic <- function(nvars = 3, bias.raster, bias.strength = 1, npoints = 100, outfile, 
                                  n.background = 1000, buffer.width = 100000, allopatry = allopatry, ...){
  outhtml <- paste0(outfile, ".html")
  knit2html('./sim.logistic.Rmd', outfile, force_v1 = TRUE)
}


