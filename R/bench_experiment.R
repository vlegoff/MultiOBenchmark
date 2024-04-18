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

options(batchtools.progress=FALSE)

parser = ArgumentParser()

parser$add_argument("-r", "--registry", nargs=1,
    help="Name of registry to use. Will be create if nonexistent. A
    corresponding config file must already exist")
parser$add_argument("-b", "--batchconf",
    help="path to batchtools configuration file")
parser$add_argument("-N", "--Ncpus", default=1,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-s", "--seed", default=1234,
    help="seed used to create resampling folds [default %(default)s]")
parser$add_argument("--resubmit_errors", action="store_true",
    help="")
parser$add_argument("--resubmit_not_finished", action="store_true",
    help="")
parser$add_argument("--pegasus", action="store_true",
    help="")

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

if(!args$pegasus) {
    resources = resources[names(resources)!="pegasus"]
    lapply(names(resources),
        function(l) submitLrn(l,
            jobs_per_chunk=resources[[l]]$jobs_per_chunk,
            resources=resources[[l]],
            resubmit_errors=args$resubmit_errors,
            resubmit_not_finished=args$resubmit_not_finished))
} else {
    pegasus_dir = paste0(reg$file.dir, "/pegasus")
    if (dir.exists(pegasus_dir)) {
        unlink(pegasus_dir, recursive=TRUE)
    }
    dir.create(pegasus_dir)

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

    brew_env = new.env()
    brew_env$resources = resources[["pegasus"]]
    brew_env$job = list(
        registry=args$registry,
        dag=base::normalizePath(paste0(reg$file.dir, "/pegasus/tasks.dag"))
    )
    brew::brew(file="hpc_templates/pegasus.tmpl",
        output=paste0(pegasus_dir, "/launcher.sh"),
        envir=brew_env)

    system("module load pegasus")
    system(paste0("ccc_msub ", pegasus_dir, "/launcher.sh"))
}

saveRegistry(reg)
