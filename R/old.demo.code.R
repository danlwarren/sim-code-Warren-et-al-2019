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

setwd("~/GitHub/SDM-Sim/")

source("./R/clusterdist.R")
source("./R/background.buffer.R")
source("./R/raster.limit.R")
source("./R/multispecies.table.background.R")
source("./R/moses.species.R")
source("./R/print.moses.species.R")
source("./R/add.env.R")
source("./R/moses.kfold.R")
source("./R/merge.species.pa.R")
source("./R/project.and.eval.R")
source("./R/moses.clade.R")
source("./R/evolve.clade.R")
source("./R/clade.pa.R")
source("./R/allopatrify.R")
source("./R/get.clades.R")
source("./R/stack.overlap.R")
source("./R/moses.disperse.R")
source("./R/clade.pa.R")
source("./R/expand.range.R")
source("./R/clade.background.R")
source("./R/raster.cor.R")
source("./R/multi.enm.glm.R")
source("~/GitHub/virtualspecies/R/convertToPA.R")


# Grabbing the first 19 Bioclim variables
# Note you can also grab from online using getData
# e.g, worldclim <- getData("worldclim", var = "bio", res = 10)
# that's 10 minute resolution, you can also do .5, 2.5, or 5.  Shit gets big though.
env.list <- list.files(path='~/GitHub/SDM-Sim/CliMond/CM10_1975H_Bio_ASCII_V1.2/', full.names=TRUE)
env <- stack(env.list[1:19])
names(env) <- paste(rep("bio", 19), 1:19, sep="")

# Grabbing Oz outline.  Note, you can also use getData function from GADM to retrieve country boundaries.
# e.g., USA <- getData("GADM", country="USA", level=1)
aus <- readRDS('~/GitHub/SDM-Sim/AUS_adm0.rds') 

# Simplifying the Oz shapefile because it's effin' huge
aus <- gSimplify(aus, tol=.01)

# Cropping and masking CliMond to Oz
env <- crop(env, extent(aus))
env <- mask(env, aus)


# Making unicorns from sample code at http://borisleroy.com/files/virtualspecies-tutorial.html#input-data
my.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 250, sd = 400),
                                 bio12 = c(fun = 'dnorm', mean = 1500, sd = 400))

# Generation of the virtual species
my.first.species <- generateSpFromFun(raster.stack = env[[c("bio1", "bio12")]],
                                      parameters = my.parameters,
                                      plot = TRUE)

# Is this probabilisitic?  May need to make it deterministic for this purpose.
pa <- convertToPA(my.first.species, beta = 0.5, alpha = -0.00007, plot = TRUE)


# Simulate some allopatry
fake.sp <- clusterdist(pa, maxclust=5, env, plotty=TRUE)

# Extract point data
fake.points <- data.frame(rasterToPoints(fake.sp))
colnames(fake.points) <-  c("lon", "lat", "species")
fake.points$species <- paste("species", fake.points$species, sep=".")
fake.points$species <- as.factor(fake.points$species)

# Let's subsample those points so we don't have full representation
fake.points <- fake.points[sample(1:nrow(fake.points), 500),]

fake.points <- multispecies.table.background(fake.points, env, 350000, 0.2)



# Make some moses.species objects
species.1.range <- fake.sp == 1
species.1.range[species.1.range == 0] <- NA

species.1 <- moses.species(virtualspecies = my.first.species,
                           actual.range = species.1.range,
                           presence.points = fake.points[fake.points$species == "species.1" & fake.points$pres == 1,],
                           background.points = fake.points[fake.points$species == "species.1" & fake.points$pres == 0,],
                           species.name = "species.1")

species.2.range <- fake.sp == 2
species.2.range[species.1.range == 0] <- NA

species.2 <- moses.species(virtualspecies = my.first.species,
                           actual.range = species.2.range,
                           presence.points = fake.points[fake.points$species == "species.2" & fake.points$pres == 1,],
                           background.points = fake.points[fake.points$species == "species.2" & fake.points$pres == 0,],
                           species.name = "species.2")

species.3.range <- fake.sp == 3
species.3.range[species.1.range == 0] <- NA

species.3 <- moses.species(virtualspecies = my.first.species,
                           actual.range = species.3.range,
                           presence.points = fake.points[fake.points$species == "species.3" & fake.points$pres == 1,],
                           background.points = fake.points[fake.points$species == "species.3" & fake.points$pres == 0,],
                           species.name = "species.3")

# Print summaries
species.1
species.2
species.3

my.species <- list(species.1 = species.1, species.2 = species.2, species.3 = species.3)
my.species <- lapply(my.species, function(x) add.env(x, env))
my.species <- lapply(my.species, function(x) moses.kfold(x, 5))



# Build glms for species in my.species
my.glms <- multi.enm.glm(my.species, env)

# Load in a future climate scenario
env2 <- stack("~/GitHub/SDM-Sim/Australia CliMond/CM10_2100_A2_CS_Bio_ASCII_V1.gri")

# Project and evaluate models to future climate
proj.2100 <- project.and.eval(my.glms, env2)
plot(stack(proj.2100$predicted))
plot(stack(proj.2100$true))
proj.2100$predction.table

# Evolve a clade from a starting species
# Making unicorns from sample code at http://borisleroy.com/files/virtualspecies-tutorial.html#input-data
root.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 14, sd = 5),
                                   bio12 = c(fun = 'dnorm', mean = 1200, sd = 200))

# Generation of the virtual species
root.species <- generateSpFromFun(raster.stack = env[[c("bio1", "bio12")]],
                                  parameters = root.parameters,
                                  plot = TRUE)

# Evolve a clade
my.clade <- evolve.clade(root.species, ntaxa = 10, env = env, rate = 0.2, alpha=0.5)


# Get allopatric ranges/suitabilities for sampling occurrences
# NOTE: modeify allopatrify so that it sticks stuff in sample.stack into species $actual.range vars instead
# and returns a clade
test.allopatric.pa <- allopatrify(my.clade, raster.template = env[[1]], buffer.width = 1)

test.pa <- clade.pa(test.allopatric.pa, sample.source = "actual.range", npres=100)$clade
test.background <- clade.background(x = test.pa,env=env,buffer.dist=200000,prevalence=0.1)
test.background$species <- lapply(test.background$species, function(x) add.env(x, env))
test.background$species <- lapply(test.background$species, function(x) moses.kfold(x, 5))

test.glm <- multi.enm.glm(test.background$species, env)

test.project <- project.and.eval(test.glm, env2)

# For each column, which model predicts the best?
rownames(test.project$prediction.table[2:11,])[apply(test.project$prediction.table[2:11,], 2, which.max)]
apply(test.project$prediction.table, 1, mean)
apply(test.glm$test.pred.table, 1, mean)
apply(test.glm$train.pred.table, 1, mean)
