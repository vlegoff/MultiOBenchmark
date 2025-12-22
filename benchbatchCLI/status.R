suppressMessages({
    library(benchbatch)
    library(batchtools)
    library(data.table)
    library(argparse)
})

parser = ArgumentParser()
parser$add_argument("-r", "--registry", nargs=1,
    help="Name of registry to extract results from. The registry must exist")
parser$add_argument("-m", "--message", action="store_true", default=FALSE,
    help="print error messages")
args = parser$parse_args()

registry_dir = args$registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=F)
} else {
    stop("registry must exists")
}

benchmark_status(reg, error_messages=args$message)
