library(mlr)
library(batchtools)

reg <- makeExperimentRegistry("BatchToolsRegistry",
    packages=c("mlr", "survival"))
reg$cluster.functions <- makeClusterFunctionsSlurm(template="slurm-simple")

nams <- list.files("../rawdata", full.names=T)

dats <- sapply(nams, function(x) load(x))

