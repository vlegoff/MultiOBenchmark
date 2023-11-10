library(mlr3verse)
library(mlr3proba)
library(mlr3batchmark)
library(batchtools)
library(data.table)

# Setting variables for cluster
registry_name <- "BatchToolsRegistry"
packages <- c("mlr3verse", "mlr3proba")
template <- "./slurm-simple.tmpl"

# Loading batchtools registry
if (dir.exists(registry_name)) {
    reg <- loadRegistry(registry_name, writeable=T)
} else {
    reg <- makeExperimentRegistry(registry_name, packages=packages)
}
#reg$cluster.functions <- makeClusterFunctionsSlurm(template=template,
    #array.jobs=TRUE, nodename="inti.cnrgh.fr")
reg$packages <- packages
saveRegistry(reg)

# defining datasets to load
datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
    "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM", "STAD", "UCEC")
datasets <- c("ESCA", "LUSC")

# load datasets
dats <- lapply(paste0("../rawdata/", datasets, ".csv"), fread)
names(dats) <- datasets

# Defining the tasks
tasks <- lapply(datasets,
                function(x) {
                    as_task_surv(dats[[x]],
                                 event="status", time="time",
                                 type="right", id=x)
                    })
names(tasks) <- datasets

cols <- colnames(dat)
blocks <- sapply(strsplit(cols, "_", fixed=TRUE), tail, 1)

set.seed(123)
learners <- list(
    lrn("surv.kaplan", id="kaplan"),
    as_learner(
        po("select", selector=selector_grep("clinical$")) %>>%
        lrn("surv.coxph"),
        id="clin_ref"
    )
)
cv10 <- rsmp("cv", folds=10)
design <- benchmark_grid(tasks, learners, cv10)

batchmark(design, reg=reg)

summarizeExperiments(by=c("task_id", "learner_id"))

resources <- list(walltime=60*60, ntasks=1, ncpus=1, memory=2000)
ids <- findNotSubmitted()
submitJobs(ids, resources=resources)
getStatus()

error_ids <- findErrors()
summarizeExperiments(error_ids, by=c("task_id", "learner_id"))

bmr <- reduceResultsBatchmark()

bmrdt <- as.data.table(bmr, measure="surv.cindex")

autoplot(bmr, measure=msr("surv.cindex"))

autoplot(bmr, measure=msr("surv.brier"))
