suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3batchmark)
    library(mlr3extralearners)
    library(batchtools)
    library(data.table)
    library(argparse)
    library(parallel)
    source("R/misc/bench_misc.R")
})

parser = ArgumentParser()

parser$add_argument("-r", "--registry", nargs=1,
    help="Name of registry to use. Will be create if nonexistent. A
    corresponding config file must already exist")
parser$add_argument("-b", "--batchconf",
    help="path to batchtools configuration file")
parser$add_argument("-N", "--Ncpus", default=1,
    help="number of cpu to use [default %(default)s]")
#parser$add_argument("-m", "--mode", default="i", nargs=1,
    #help="Batchtools Cluster Function: i for interactive, m for multicore, s
    #for slurm [default %(default)s]")
#parser$add_argument("-t", "--template", default="slurm-simple",
    #help="template to use for slurm cluster of batchtools [default
    #%(default)s]")
#parser$add_argument("--hostname", default="inti.cnrgh.fr",
    #help="hostname of the cluster frontend node [default %(default)s]")
parser$add_argument("-s", "--seed", default=1234,
    help="seed used to create resampling folds [default %(default)s]")

args = parser$parse_args()

set.seed(args$seed)

#ncpus = as.numeric(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))
#ncpus = if (is.na(ncpus)) parallelly::availableCores() else ncpus
data.table::setDTthreads(args$Ncpus)

registry_dir = paste0("registries/", args$registry)
config_file = paste0("configs/", args$registry, ".config.R")

# Loading batchtools registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
    source(args$batchconf)
    reg$cluster.functions = cluster.functions
    reg$packages = packages
} else {
    reg <- makeExperimentRegistry(registry_dir, conf.file=args$batchconf)
}
reg$work.dir <- getwd()

source(config_file)

names(learners) = sapply(learners, `[[`, "id")

# load datasets
dats <- lapply(paste0("data/", datasets, ".csv"), fread)
names(dats) <- datasets

# Defining the tasks
tasks <- mclapply(datasets,
    function(x) {
        tsk <- as_task_surv(dats[[x]],
            event="status", time="time",
            type="right", id=x)
        tsk$add_strata("status")
        tsk
    },
    mc.cores=args$Ncpus)
names(tasks) <- datasets

cv10 <- rsmp("repeated_cv", repeats=10, folds=5)
design <- benchmark_grid(tasks, learners, cv10)

batchmark(design, store_models=FALSE, reg=reg)

lapply(names(resources),
    function(l) submitLrn(l,
        jobs_per_chunk=resources[[l]]$jobs_per_chunk,
        resources=resources[[l]],
        not_done=TRUE))

#mlr3extralearners::create_learner(
    #path="learners",
    #classname="rgcca",
    #type="surv",
    #key="rgcca",
    #algorithm="RGCCA for dimension reduction followed by glmnet for surv prediction",
    #package=c("RGCCA"),
    #caller=c("RGCCA"),
    #feature_types=c("integer", "numeric", "factor"),
    #predict_types=c("crank", "lp", "distr"),
    #properties=c("weights"),
    #label="Regularized Generalized Canonical Correlation Analysis for Survival"
#)
