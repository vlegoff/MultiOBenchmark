suppressMessages({
    library(data.table)
    library(batchtools)
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3batchmark)
    library(mlr3extralearners)
    library(argparse)
    library(parallel)
    source("R/misc/bench_misc.R")
})

options(batchtools.progress=FALSE)

parser = ArgumentParser()

parser$add_argument("-l", "--learners", nargs=1,
    help="config file containing the learners to run", required=TRUE)
parser$add_argument("-d", "--data", nargs=1,
    help="config file containing the description of the datasets on which
    to run the learners", required=TRUE)
parser$add_argument("-r", "--registry", nargs=1,
    help="path to the batchtools registry to use.
    Will be created if it does not exists", required=TRUE)
parser$add_argument("-N", "--Ncpus", default=1L, type="integer", choices=1:36,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-s", "--seed", default=1234,
    help="seed used to create resampling folds [default %(default)s]")

# when no resources/no batchtools, just launch with interactive mode

args = parser$parse_args()

set.seed(args$seed)

data.table::setDTthreads(args$Ncpus)

registry_dir = args$registry

#sourcing config files
source(args$learners)
source(args$data)

# Loading batchtools registry
if (dir.exists(registry_dir)) reg <- loadRegistry(registry_dir, writeable=T)
else reg <- makeExperimentRegistry(registry_dir)
reg$work.dir <- getwd()

names(learners) = sapply(learners, `[[`, "id")

# load datasets
for (d in names(datasets)) {
    if (!all(c("target", "path") %in% names(datasets[[d]]))) {
        stop(paste0("dataset ", d, " lacks either target or path"))
    }
    datasets[[d]]$x = fread(datasets[[d]]$path)
    if (!all(datasets[[d]]$target %in% colnames(datasets[[d]]$x))) {
        stop(paste0("targets ",
                paste(datasets[[d]]$target, collapse=" "), 
                " are not in dataset ", d))
    }
}

task_functions = list(
    survival=as_task_surv,
    regression=as_task_regr,
    classification=as_task_classif
)

if (task_type %in% names(task_functions)) {
    create_task = task_functions[[task_type]]
} else if (task_type %in% c("density", "clustering")) {
    stop(paste0("task type ", task_type, " not yet implemented"))
} else {
    stop(paste0("task type ", task_type, " not recognized"))
}

# Defining the tasks
# check that all targets are in the dataset outside of mclapply?
tasks <- mclapply(names(datasets),
    function(d) {
        parms = if (task_type == "survival") {
            list(
                x=datasets[[d]]$x,
                time=datasets[[d]]$target[1],
                event=datasets[[d]]$target[2],
                type="right",
                id=d
            )
        } else if (task_type=="regression" | task_type=="classification") {
            list(
                x=datasets[[d]]$x,
                target=datasets[[d]]$target,
                id=d
            )
        } else {
            stop("wtf!")
        }
        tsk = mlr3misc::invoke(create_task, .args=parms)
        for (s in datasets[[d]]$strata) {
            tsk$add_strata(s)
        }
        tsk
    },
    mc.cores=args$Ncpus)
names(tasks) <- names(datasets)

paired_cv = ifelse(!exists("paired_cv"), F, paired_cv)
store_models = ifelse(!exists("store_models"), F, store_models)
# check if cv has been defined in config
# mainly for backward compatibility
if(!exists("cv")) {
    cv = rsmp("repeated_cv", repeats=10, folds=5)
} else if (cv$id=="custom") {
    if(!paired_cv) stop("cv is custom but not paired")
    if(!exists("folds")) stop("cv is custom but no folds are provided")
    if(!length(folds)==length(datasets)) stop("wrong number of resamplings")
    cv = vector("list", length=length(tasks)) 
    for(i in seq_along(tasks)) {
        #d = datasets[i]
        d = names(tasks)[i]
        cv[[i]] = rsmp("custom")$instantiate(tasks[[d]],
            train=folds[[d]]$train, test=folds[[d]]$test)
    }
} else {
    cv = cv
}

design <- benchmark_grid(tasks, learners, cv, paired=paired_cv)
batchmark(design, store_models=store_models, reg=reg)
saveRegistry(reg)
