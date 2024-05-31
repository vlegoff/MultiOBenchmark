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
parser$add_argument("--rds", action="store_true", default=FALSE,
    help="store results to rds")

#rds = FALSE # add it as a parser argument

args = parser$parse_args()

# Setting variables for cluster
data.table::setDTthreads(args$Ncpus)

#registry_dir = paste0("registries/", args$registry)
registry_dir = args$registry
#config_file = paste0("configs/", args$registry, ".config.R")

source(args$measures)

#res_dir = paste0(args$directory, "/", args$registry)
res_dir = args$directory
res_file = tail(strsplit(args$directory, "/")[[1]], 1)
if (!dir.exists(res_dir)) dir.create(res_dir)

# Loading batchtools registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
} else {
    stop("registry must exists")
}
reg$work.dir <- getwd()

#source(config_file)

done = findDone()$job.id
done = split(done, ceiling(seq_along(done)/args$chunk_size))
bmrdt = vector("list", length(done))
for (i in seq_along(done)) {
    print(sprintf("%d/%d chunks", i, length(done)))
    bmr <- reduceResultsBatchmark(done[[i]], store_backends=TRUE)
    if (args$rds)
        saveRDS(bmr, sprintf("%/s/%s_results_%06d.rds", res_dir, res_file,
                             i))
    cols = c("uhash", "nr", "task_id", "learner_id", "resampling_id",
             "iteration", sapply(measures, `[[`, "id"))
    bmrdt[[i]] = bmr$score(measures)[,..cols] 
}
bmrdt = rbindlist(bmrdt)

if(nrow(findErrors()) > 0) {
    err = unwrap(getJobTable(findErrors()))[,
        .(job.name, repl, task_id, learner_id, resampling_id, error)
    ]
    colnames(err)[colnames(err) == "job.name"] = "uhash"
    colnames(err)[colnames(err) == "repl"] = "iteration"
    bmrdt = rbindlist(list(bmrdt, err), fill=TRUE)
    # fill=TRUE sould not be necessary now, remove it
}

fwrite(bmrdt, sprintf("%s/%s_results.csv", res_dir, res_file))
