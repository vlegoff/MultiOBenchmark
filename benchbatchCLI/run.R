suppressMessages({
    library(data.table)
    library(benchbatch)
    library(batchtools)
    library(argparse)
    library(parallel)
})

parser = ArgumentParser()
parser$add_argument("-m", "--mode", 
    choices=c("interactive", "multicore", "slurm", "pegasus"),
    default=NULL, help="how to execute jobs")
parser$add_argument("-c", "--resources", nargs=1,
    help="resources with which to launch the benchmark", required=FALSE)
parser$add_argument("-r", "--registry", nargs=1,
    help="path to the batchtools registry to use.
    Will be created if it does not exists", required=TRUE)
parser$add_argument("-N", "--Ncpus", default=1L, type="integer", choices=1:36,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("--resubmit_errors", action="store_true",
    help="jobs that ended in errors are resubmitted")
parser$add_argument("--resubmit_not_finished", action="store_true",
    help="jobs that are not finished are resubmitted.
    WARNING: this may resubmit jobs that are still running 
    and I don't know what happens in this case.")
args = parser$parse_args()

set.seed(args$seed)

data.table::setDTthreads(args$Ncpus)

registry_dir = args$registry

# Loading batchtools registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
} else {
    stop("registry must already exists")
}

if (args$mode=="interactive") {
    resources=NULL
} else if (is.null(args$resources)) {
    if (args$mode=="multicore" & args$Ncpus>1) {
        resources = list(ncpus=args$Ncpus)
    } else {
        warning("no resource file provided, defaulting to interactive mode")
        args$mode = "interactive"
    }
} else resources = parse_resources(args$resources)

run(reg, cluster=args$mode, resources=resources,
    resubmit_not_finished=args$resubmit_not_finished,
    resubmit_errors=args$resubmit_errors)
