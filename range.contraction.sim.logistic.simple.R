library(ggplot2)
source("mod.randomPoints.R")
library(rmarkdown)
library(dismo)
library(randomForest)
library(mgcv)

range.contraction.sim.logistic.simple <- function(nvars = 2, bias.raster, bias.strength = 1, npoints = 100, outfile, 
                                  n.background = 1000, buffer.width = 100000, allopatry = allopatry, ...){
  outhtml <- paste0(outfile, ".html")
  knit2html('./range.contraction.sim.logistic.simple.Rmd', outfile, force_v1 = TRUE)
}


