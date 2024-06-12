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

parser$add_argument("-r", "--registry", nargs=1,
    help="path to the batchtools registry to use.
    Will be created if it does not exists", required=TRUE)
parser$add_argument("-c", "--resources", nargs=1,
    help="resources with which to launch the benchmark", required=FALSE)
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
    help="For CNRGH internal use only, bypass batchtools to submits jobs 
    through pegasus, an MPI microscheduler")

# when no resources/no batchtools, just launch with interactive mode

args = parser$parse_args()

set.seed(args$seed)

data.table::setDTthreads(args$Ncpus)

registry_dir = args$registry

#sourcing config files
if (!is.null(args$resources)) source(args$resources)

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
        if (args$Ncpus > 1) {
            reg$cluster.functions = makeClusterFunctionsMulticore(args$Ncpus)
        } else {
            reg$cluster.functions = makeClusterFunctionsInteractive()
        }
    }
} else stop("registry must exist!")
reg$work.dir <- getwd()

if (!reg$cluster.functions$name %in% c("Interactive", "Multicore")
    & is.null(args$resources)) {
    stop(paste0("Must provide a resource file with cluster functions ", 
        cluster.functions$name))
}

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
    if (reg$cluster.functions$name == "Multicore") waitForJobs()

} else if (!args$pegasus) {

    resources = resources[names(resources)!="pegasus"]
    lapply(names(resources),
        function(l) submitLrn(l,
            jobs_per_chunk=resources[[l]]$jobs_per_chunk,
            resources=resources[[l]],
            resubmit_errors=args$resubmit_errors,
            resubmit_not_finished=args$resubmit_not_finished))

} else { #bypassing batchtools and creating pegasus DAG

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
        registry=basename(args$registry),
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
