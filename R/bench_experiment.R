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
# resources should not be mandatory, but will do for now
parser$add_argument("-c", "--resources", nargs=1,
    help="resources with which to launch the benchmark", required=FALSE)
parser$add_argument("-r", "--registry", nargs=1,
    help="path to the batchtools registry to use.
    Will be created if it does not exists", required=TRUE)
# same as for resources
parser$add_argument("-b", "--batchconf",
    help="path to batchtools configuration file", required=FALSE)
parser$add_argument("-N", "--Ncpus", default=1L, type="integer", choices=1:36,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-s", "--seed", default=1234,
    help="seed used to create resampling folds [default %(default)s]")
parser$add_argument("--resubmit_errors", action="store_true",
    help="jobs that ended in errors are resubmitted")
parser$add_argument("--resubmit_not_finished", action="store_true",
    help="jobs that have not finished are resubmitted.
    WARNING: this may resubmit jobs that are still running 
    and I don't know what happens in this case.")
parser$add_argument("--pegasus", action="store_true",
    help="")

# when no resources/no batchtools, just launch with interactive mode

args = parser$parse_args()

set.seed(args$seed)

data.table::setDTthreads(args$Ncpus)

registry_dir = args$registry
#config_file = paste0("configs/", args$registry, ".config.R")

#sourcing config files
source(args$learners)
source(args$data)

# Loading batchtools registry
# make batchconf 
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
    if (!is.null(args$batchconf)) {
        source(args$batchconf)
        reg$cluster.functions = cluster.functions
        reg$packages = packages
    } else {
        reg$packages = c("mlr3", "mlr3proba", "data.table")
    }
} else {
    if (!is.null(args$batchconf)) {
        reg <- makeExperimentRegistry(registry_dir, conf.file=args$batchconf)
    } else {
        reg <- makeExperimentRegistry(registry_dir)
        print(args$Ncpus)
        if (args$Ncpus > 1) {
            reg$cluster.functions = makeClusterFunctionsMulticore(args$Ncpus)
        }
        reg$packages = c("mlr3", "mlr3proba", "data.table")
    }
}
reg$work.dir <- getwd()

if (!reg$cluster.functions$name %in% c("Interactive", "Multicore")
    & is.null(args$resources)) {
    stop(paste0("Must provide a resource file with cluster functions ", 
        cluster.functions$name))
}

names(learners) = sapply(learners, `[[`, "id")

# load datasets
for (d in names(datasets)) {
    if (!all(c("target", "path") %in% names(datasets[[d]]))) {
        stop(paste0("dataset ", d, " lacks either target or path"))
    }
    datasets[[d]]$x = fread(datasets[[d]]$path)
    if (!all(datasets[[d]]$target %in% colnames(datasets[[d]]$x))) {
        stop(paste0("targets", paste(targets), "are not in dataset ", d))
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
    for(i in seq_along(datasets)) {
        d = datasets[i]
        cv[[i]] = rsmp("custom")$instantiate(tasks[[d]],
            train=folds[[d]]$train, test=folds[[d]]$test)
    }
} else {
    cv = cv
}

design <- benchmark_grid(tasks, learners, cv, paired=paired_cv)
batchmark(design, store_models=store_models, reg=reg)

# Submitting jobs either through batchtools or pegasus
if(!args$pegasus &
    reg$cluster.functions$name %in% c("Interactive", "Multicore")) { 

    ids = findNotSubmitted()
    err =  findErrors()
    not_finished = findNotDone()[
        !(job.id %in% ids$job.id | job.id %in% err$job.id)
    ]
    if (args$resubmit_errors) ids = rbind(ids, err)
    if (args$resubmit_not_finished) ids = rbind(ids, not_finished)
    submitJobs(ids=ids, reg=reg)

} else if (!args$pegasus) {

    source(args$resources)
    resources = resources[names(resources)!="pegasus"]
    lapply(names(resources),
        function(l) submitLrn(l,
            jobs_per_chunk=resources[[l]]$jobs_per_chunk,
            resources=resources[[l]],
            resubmit_errors=args$resubmit_errors,
            resubmit_not_finished=args$resubmit_not_finished))

} else { #bypassing batchtools and creating pegasus DAG

    source(args$resources)
    pegasus_dir = paste0(reg$file.dir, "/pegasus")
    if (dir.exists(pegasus_dir)) {
        unlink(pegasus_dir, recursive=TRUE)
    }
    dir.create(pegasus_dir)

    # getting jobs to run
    ids = findNotSubmitted()
    err =  findErrors()
    not_finished = findNotDone()[
        !(job.id %in% ids$job.id | job.id %in% err$job.id)
    ]
    if (args$resubmit_errors) ids = rbind(ids, err)
    if (args$resubmit_not_finished) ids = rbind(ids, not_finished)
    jobs = getJobTable(ids)
    if(nrow(jobs) == 0) {
        stop("nothing to run")
    }

    #manually creating batchtools rds and pegasus DAG to run them
    dag = vector("character", nrow(jobs))
    for (i in 1:nrow(jobs)) {
        collection = makeJobCollection(jobs[i, job.id])
        collection_file = paste0(reg$file.dir, "/pegasus/",
            collection$job.hash[1], ".rds")
        if(is.na(jobs[i, job.hash]))
            jobs[i, job.hash := collection$job.hash[1]]
        saveRDS(collection, collection_file)
        dag[i] = paste0("TASK ", collection$job.hash[1], " -c 1",
            " bash -Eeo pipefail -c ",
            "\"Rscript -e \\\'batchtools::doJobCollection(\\\"", 
            collection_file, "\\\")\\\'\"")
        reg$status[i, submitted := as.numeric(Sys.time())]
    }
    fileConn = file(paste0(reg$file.dir, "/pegasus/tasks.dag"))
    writeLines(dag, fileConn)
    close(fileConn)

    #creating shell script to run pegasus with asked resources
    brew_env = new.env()
    brew_env$resources = resources[["pegasus"]]
    brew_env$job = list(
        registry=args$registry,
        dag=base::normalizePath(paste0(reg$file.dir, "/pegasus/tasks.dag"))
    )
    brew::brew(file="hpc_templates/pegasus.tmpl",
        output=paste0(pegasus_dir, "/launcher.sh"),
        envir=brew_env)

    #Running pegasus, specific to INTI, maybe works at TGCC
    system("module load pegasus")
    system(paste0("ccc_msub ", pegasus_dir, "/launcher.sh"))

}

saveRegistry(reg)
