library(virtualspecies)
library(dismo)
library(raster)
library(rgeos)
library(cluster)
library(fields)
library(ape)
library(MASS)
library(geiger)
library(plyr)
library(MigClim)
library(gdistance)
library(rmarkdown)
library(knitr)
library(ggplot2)


source("~/GitHub/SDM-sim/R/clusterdist.R")
source("~/GitHub/SDM-sim/R/background.buffer.R")
source("~/GitHub/SDM-sim/R/raster.limit.R")
source("~/GitHub/SDM-sim/R/multispecies.table.background.R")
source("~/GitHub/SDM-sim/R/moses.species.R")
source("~/GitHub/SDM-sim/R/print.moses.species.R")
source("~/GitHub/SDM-sim/R/add.env.R")
source("~/GitHub/SDM-sim/R/moses.kfold.R")
source("~/GitHub/SDM-sim/R/merge.species.pa.R")
source("~/GitHub/SDM-sim/R/project.and.eval.R")
source("~/GitHub/SDM-sim/R/moses.clade.R")
source("~/GitHub/SDM-sim/R/evolve.clade.R")
source("~/GitHub/SDM-sim/R/clade.pa.R")
source("~/GitHub/SDM-sim/R/allopatrify.R")
source("~/GitHub/SDM-sim/R/get.clades.R")
source("~/GitHub/SDM-sim/R/stack.overlap.R")
source("~/GitHub/SDM-sim/R/moses.disperse.R")
source("~/GitHub/SDM-sim/R/clade.pa.R")
source("~/GitHub/SDM-sim/R/expand.range.R")
source("~/GitHub/SDM-sim/R/clade.background.R")
source("~/GitHub/SDM-sim/R/raster.cor.R")
source("~/GitHub/SDM-sim/R/multi.enm.glm.R")
source("~/GitHub/SDM-sim/R/unlist.args.R")
source("~/GitHub/SDM-sim/R/summarize.clade.R")
source("~/GitHub/SDM-sim/R/summarize.multi.glm.R")
source("~/GitHub/virtualspecies/R/convertToPA.R")

