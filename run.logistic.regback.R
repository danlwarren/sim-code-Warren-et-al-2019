setwd("~/GitHub/SDM-sim/")
source("./R/SDM-sim.R")
source("sim.logistic.regback.R")

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

# Somewhere along the way this gets changed from a stack to a brick, which causes biomod to shit itself
env <- stack(env)

# Grabbing bias data for Oz plants
bias.raster <- raster("Plant Famillies Bias.grd")
bias.raster <- bias.raster/cellStats(bias.raster, stat = "max")


reps <- seq(1, 20)
bias.levels <- seq(0, 1, by=0.1)

test.grid <- expand.grid(reps, bias.levels)
test.grid <- test.grid[sample(1:nrow(test.grid)),]

for(i in 1:nrow(test.grid)){
  bias.strength <- test.grid[i, 2]
  rep <- test.grid[i,1]
  if(rep < 3){allopatry = FALSE}
  else{allopatry = TRUE}
  
  path <- paste0("./logistic sims regback/bias_", bias.strength, "_rep_", rep, "/")
  dir.create(path, showWarnings = TRUE, recursive = FALSE)
  
  outfile <- paste0(path, "bias_", bias.strength, "_rep_", rep)
  
  print(outfile)
  
  sim.logistic(bias.raster = bias.raster, outfile = outfile, replace = TRUE, 
                        bias.strength = bias.strength, allopatry = allopatry)
}






sim.logistic(bias.raster = bias.raster, outfile = "./logistic sims regback", replace = TRUE, bias.strength = 0.2, allopatry = TRUE)


