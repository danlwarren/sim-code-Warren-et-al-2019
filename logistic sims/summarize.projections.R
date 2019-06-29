setwd("~/GitHub/SDM-Sim/logistic sims")
library(ggplot2)

sims.table <- read.csv("./detail.table.csv")

sims.table$true.change.all	<- rep(NA, nrow(sims.table))
sims.table$pred.change.all		<- rep(NA, nrow(sims.table))
sims.table$pred.error.all		<- rep(NA, nrow(sims.table))
sims.table$pred.cor.all		<- rep(NA, nrow(sims.table))
sims.table$true.change.occ		<- rep(NA, nrow(sims.table))
sims.table$pred.change.occ		<- rep(NA, nrow(sims.table))
sims.table$pred.error.occ		<- rep(NA, nrow(sims.table))
sims.table$pred.cor.occ		<- rep(NA, nrow(sims.table))
sims.table$true.cells.declining.all		<- rep(NA, nrow(sims.table))
sims.table$pred.cells.declining.all		<- rep(NA, nrow(sims.table))
sims.table$prop.agreed.declining.all		<- rep(NA, nrow(sims.table))
sims.table$true.cells.declining.occ		<- rep(NA, nrow(sims.table))
sims.table$pred.cells.declining.occ		<- rep(NA, nrow(sims.table))
sims.table$prop.agreed.declining.occ	<- rep(NA, nrow(sims.table))

for(i in 1:nrow(sims.table)){
  this.infile <- paste0("./", sims.table[i,"sim"], "/", sims.table[i, "sim"], ".projection.summary.csv")
  if(file.exists(this.infile)){
    print(paste("Found", this.infile))
    this.proj <- read.csv(this.infile)
    method <- strsplit(as.character(this.proj$model), split = "\\.")
    if(sims.table[i, "bias.strength"] == 0 | sims.table[i, "bias.strength"] == 1){
      method <- unlist(lapply(method, function(x) x[3]))
    } else {
      method <- unlist(lapply(method, function(x) x[5]))
    }
    this.proj$method <- method
    print(sims.table[i,"method"])
    print(method[1:4])
    sims.table[i,"true.change.all"] <- mean(this.proj[this.proj$method == sims.table[i,"method"],"true.change.all"])
    sims.table[i,"pred.change.all"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.change.all"])
    sims.table[i,"pred.error.all"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.error.all"])
    sims.table[i,"pred.cor.all"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.cor.all"])
    sims.table[i,"true.change.occ"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"true.change.occ"])
    sims.table[i,"pred.change.occ"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.change.occ"])
    sims.table[i,"pred.error.occ"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.error.occ"])
    sims.table[i,"pred.cor.occ"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.cor.occ"])
    sims.table[i,"true.cells.declining.all"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"true.cells.declining.all"])
    sims.table[i,"pred.cells.declining.all"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.cells.declining.all"])
    sims.table[i,"prop.agreed.declining.all"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"prop.agreed.declining.all"])
    sims.table[i,"true.cells.declining.occ"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"true.cells.declining.occ"])
    sims.table[i,"pred.cells.declining.occ"]	<- mean(this.proj[this.proj$method == sims.table[i,"method"],"pred.cells.declining.occ"])
    sims.table[i,"prop.agreed.declining.occ"]<- mean(this.proj[this.proj$method == sims.table[i,"method"],"prop.agreed.declining.occ"])
  }
}

sims.table <- sims.table[complete.cases(sims.table),]
qplot(test.auc, pred.cor.all, data = sims.table, color = method)
summary(glm(pred.cor.all ~ test.auc, data = sims.table))

qplot(test.auc, pred.cor.occ, data = sims.table, color = method)
summary(glm(pred.cor.occ ~ test.auc, data = sims.table))

qplot(test.max.kappa, pred.cor.all, data = sims.table, color = method)
summary(glm(pred.cor.all ~ test.max.kappa, data = sims.table))

qplot(test.max.kappa, pred.cor.occ, data = sims.table, color = method)
summary(glm(pred.cor.occ ~ test.max.kappa, data = sims.table))

qplot(test.max.tss, pred.cor.all, data = sims.table, color = method)
summary(glm(pred.cor.all ~ test.max.tss, data = sims.table))

qplot(test.max.tss, pred.cor.occ, data = sims.table, color = method)
summary(glm(pred.cor.occ ~ test.max.tss, data = sims.table))

qplot(pred.error.occ, data = sims.table, fill = method, geom = "density", alpha = 0.5) + geom_vline(xintercept = 0)

qplot(pred.error.all, data = sims.table, fill = method, geom = "density", alpha = 0.5) + geom_vline(xintercept = 0)

qplot(pred.change.occ, data = sims.table, fill = method, geom = "density", alpha = 0.5) + geom_vline(xintercept = 0)

qplot(pred.change.all, data = sims.table, fill = method, geom = "density", alpha = 0.5) + geom_vline(xintercept = 0)

qplot(pred.cells.declining.occ, data = sims.table, fill = method, geom = "density", alpha = 0.5) + geom_vline(xintercept = 0)

qplot(pred.cells.declining.all, data = sims.table, fill = method, geom = "density", alpha = 0.5) + geom_vline(xintercept = 0)

qplot(pred.cor.all, pred.error.all, data = sims.table, color = method)

qplot(pred.cor.occ, pred.error.occ, data = sims.table, color = method)
summary(glm(pred.error.occ ~ pred.cor.occ, data = sims.table))

qplot(test.auc, pred.error.occ, data = sims.table)
summary(glm(pred.error.occ ~ test.auc, data = sims.table))

qplot(test.auc, pred.error.all, data = sims.table)
summary(glm(pred.error.all ~ test.auc, data = sims.table))

