suppressMessages({
    library(benchbatch)
    library(batchtools)
    library(data.table)
    library(argparse)
    library(parallel)
})

parser = ArgumentParser()
parser$add_argument("-r", "--registry", nargs=1,
    help="Name of registry to extract results from. The registry must exist")
parser$add_argument("-m", "--measures", nargs=1,
    help="")
parser$add_argument("-N", "--Ncpus", default=1,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-C", "--chunk_size", default=100L, type="integer",
    help="when extracting, extract by small chunks of size C jobs
    A csv file will be created for each chunk [default $(default)s")
parser$add_argument("-d", "--directory", default="results",
    help="where to store results")
parser$add_argument("--rds", nargs=1, default=NULL,
    help="directory for storing results to rds")
parser$add_argument("-v", "--verbose", action="store_true",
    help="")
args = parser$parse_args()

data.table::setDTthreads(args$Ncpus)

# Loading batchtools registry
registry_dir = args$registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
} else {
    stop("registry must exists")
}

meas = parse_measures(args$measures)

bmrdt = collect_results(reg, meas, chunk.size=args$chunk_size, rds=args$rds,
    verbose=args$verbose)

res_dir = args$directory
res_file = tail(strsplit(args$registry, "/")[[1]], 1)
if (!dir.exists(res_dir)) dir.create(res_dir)
fwrite(bmrdt, sprintf("%s/%s_results.csv", res_dir, res_file))
