library(mlr3verse)
library(mlr3proba)
library(mlr3batchmark)
library(mlr3extralearners)
library(batchtools)
library(data.table)

# Setting variables for cluster
registry_name <- "BatchToolsRegistry"
packages <- c("mlr3verse", "mlr3proba", "data.table")
template <- "./slurm-simple.tmpl"
ncpus <- 12

data.table::setDTthreads(ncpus)

# Loading batchtools registry
if (dir.exists(registry_name)) {
    reg <- loadRegistry(registry_name, writeable=T)
} else {
    reg <- makeExperimentRegistry(registry_name, packages=packages)
}
reg$cluster.functions <- makeClusterFunctionsSlurm(template=template,
    array.jobs=TRUE, nodename="inti.cnrgh.fr")
#reg$cluster.functions <- makeClusterFunctionsMulticore(4)
reg$packages <- packages
saveRegistry(reg)

# defining datasets to load
datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
    "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM", "STAD", "UCEC")
datasets <- c("ESCA", "LUSC", "LAML", "PAAD")
#datasets <- "LAML"

# load datasets
dats <- lapply(paste0("../rawdata/", datasets, ".csv"), fread)
names(dats) <- datasets

# Defining the tasks
tasks <- lapply(datasets,
                function(x) {
                    tsk <- as_task_surv(dats[[x]],
                                        event="status", time="time",
                                        type="right", id=x)
                    tsk$add_strata("status")
                    tsk
                    })
names(tasks) <- datasets

#cols <- colnames(dat)
#blocks <- sapply(strsplit(cols, "_", fixed=TRUE), tail, 1)

source("learners/RGCCA_surv_learner.R")
source("learners/blockforest_learner.R")
source("learners/cv_prioritylasso_learner.R")

#rgcca_test = lrn("surv.rgcca", blocks=c("cnv", "mirna"), nfolds=2)
#rgcca_test$train(tasks[[3]])
#preds = rgcca_test$predict(tasks[[3]])
#preds$score(msr("surv.cindex"))

set.seed(1234)
internal_seed = 123
learners <- list(
    lrn("surv.kaplan", id="kaplan"),
    GraphLearner$new(
        po("select", selector=selector_grep("clinical$")) %>>%
        lrn("surv.coxph"),
        id="clin_ref",
    ),
    GraphLearner$new(
        po("select", selector=selector_grep("clinical$")) %>>%
        lrn("surv.cv_glmnet", alpha=0),
        id="glmnet_clin",
    ),
    lrn("surv.ranger", id="ranger", min.node.size=3),
    lrn("surv.cv_glmnet", alpha=1, id="lasso"),
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=1,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n1_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=3,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n3_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=1,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n1_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=3,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n3_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=0.1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=1,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n1_tau0.1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=0.1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=3,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n3_tau0.1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=0.1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=1,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n1_tau0.1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=c("cnv", "mirna", "mutation", "rna"),
        tau=0.1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=3,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n3_tau0.1"
    ) 
)
cv10 <- rsmp("repeated_cv", repeats=10, folds=5)
design <- benchmark_grid(tasks, learners, cv10)

batchExport(unexport=batchExport()$name) #to ensure custom learners are updated
batchmark(design, store_models=FALSE, reg=reg)

summarizeExperiments(by=c("task_id", "learner_id"))

resources <- list(walltime=60*60*24, ntasks=1, ncpus=1, memory=4000)

submitLrn <- function(lrn_id,
                      jobs_per_chunk=1,
                      resources=list(walltime=60*60, ncpus=1, memory=2000),
                      only_not_done=FALSE,
                      reg=getDefaultRegistry()) {

    ids <- unwrap(getJobTable(if (only_not_done) findNotDone(), reg=reg))[
        learner_id==lrn_id,
        job.id
    ]
    chunks <- data.table(
        job.id=ids,
        chunk=batchtools::chunk(ids, chunk.size=jobs_per_chunk, shuffle=FALSE)
    )
    submitJobs(ids=chunks, resources=resources, reg=reg)

}

submitLrn("kaplan", jobs_per_chunk=100, only_not_done=T)

submitLrn("clin_ref", jobs_per_chunk=100, only_not_done=T)

submitLrn("glmnet_clin", jobs_per_chunk=100, only_not_done=T)

submitLrn("lasso",
          jobs_per_chunk=4,
          resources=list(walltime=60*60*3, ncpus=1, memory=4000),
          only_not_done=TRUE)

submitLrn("ranger",
          jobs_per_chunk=4,
          resources=list(walltime=60*60*3, ncpus=1, memory=4000),
          only_not_done=TRUE)

submitLrn("rgcca_sup_clin_fav_n1_tau1", jobs_per_chunk=1,
          resources=list(walltime=60*60*24, ncpus=1, memory=8000),
          only_not_done=T)

# Rest
ids <- findNotSubmitted()[,job.id]
chunks <- data.table(
    job.id=ids,
    chunk=batchtools::chunk(ids, chunk.size=3, shuffle=FALSE)
)
submitJobs(ids=chunks, resources=resources)

error_ids <- findErrors()
summarizeExperiments(error_ids, by=c("task_id", "learner_id"))

bmr <- reduceResultsBatchmark(store_backends=TRUE)
saveRDS(bmr, "temp_results.rds")

bmrdt <- as.data.table(bmr, measure="surv.cindex")

bmrdt = bmr$score(list(msr("surv.cindex", weight_meth="G2"),
                       msr("surv.brier")))
fwrite(bmrdt[,.(uhash, nr, task_id, learner_id, resampling_id, iteration,
                surv.cindex, surv.graf)],
       "temp_results.csv")

png(file="autoplot_cindex.png")
autoplot(bmr, measure=msr("surv.cindex", weight_meth="G2"))
dev.off()

png(file="autoplot_brier.png")
autoplot(bmr, measure=msr("surv.brier"))
dev.off()

#autoplot(bmr, measure=msr("surv.cindex", weight_meth="G2", average="micro"))
#autoplot(bmr, measure=msr("surv.brier", average="micro"))

mlr3extralearners::create_learner(
    path="learners",
    classname="rgcca",
    type="surv",
    key="rgcca",
    algorithm="RGCCA for dimension reduction followed by glmnet for surv prediction",
    package=c("RGCCA"),
    caller=c("RGCCA"),
    feature_types=c("integer", "numeric", "factor"),
    predict_types=c("crank", "lp", "distr"),
    properties=c("weights"),
    label="Regularized Generalized Canonical Correlation Analysis for Survival"
)
