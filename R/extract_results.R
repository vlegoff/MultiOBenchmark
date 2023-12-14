.libPaths(.libPaths()[[2]]) # to ensure use of local libs

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
parser$add_argument("-N", "--Ncpus", default=1,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-C", "--chunk_size", default=100,
    help="when extracting, extract by small chunks of size C jobs
    A csv file will be created for each chunk [default $(default)s")
parser$add_argument("-d", "--directory", default=1,
    help="where to store results")

rds = FALSE # add it as a parser argument

args = parser$parse_args()

# Setting variables for cluster
data.table::setDTthreads(args$Ncpus)

registry_dir = paste0("registries/", args$registry)
config_file = paste0("configs/", args$registry, ".config.R")
res_dir = paste0(args$directory, "/", args$registry)
isdir(res_dir) || mkdir(res_dir)

# Loading batchtools registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
} else {
    stop("registry must exists")
}
reg$work.dir <- getwd()

source(config_file)

done = findDone()$job.id
done = split(done, ceiling(seq_along(done)/chunk_size))
bmrdt = vector("list", length(done))
for (i in seq_along(done)) {
    print(sprintf("%d/%d chunks", i, length(done)))
    bmr <- reduceResultsBatchmark(done[[i]], store_backends=TRUE)
    if (rds)
        saveRDS(bmr, sprintf("%/s/%s_results_%06d.rds", res_dir, reg_name, i))
    cols = c("uhash", "nr", "task_id", "learner_id", "resampling_id",
             "iteration", sapply(measures, `[[`, "id"))
    bmrdt[[i]] = bmr$score(measures)[,..cols] 
}
bmrdt = rbindlist(bmrdt)
fwrite(bmrdt, sprintf("%s/%s_results_%06d.rds", res_dir, reg_name, i))

#reduceResultsBatchmark = function(ids = NULL, store_backends = TRUE, reg = batchtools::getDefaultRegistry()) { # nolint
#  if (is.null(ids)) {
#    ids = batchtools::findDone(ids, reg = reg)
#  } else {
#    ids = batchtools::findJobs(ids = ids, reg = reg) # convert to proper table
#    if (nrow(batchtools::findNotDone(ids, reg = reg))) {
#      stop("All jobs must have been successfully computed")
#    }
#  }
#
#  tabs = batchtools::getJobTable(ids, reg = reg)[, c("job.id", "job.name", "repl", "prob.pars", "algo.pars"), with = FALSE]
#  tabs = unnest(tabs, c("prob.pars", "algo.pars"))
#  tabs = split(tabs, by = "job.name")
#  bmr = mlr3::BenchmarkResult$new()
#
#  for (tab in tabs) {
#    job = batchtools::makeJob(tab$job.id[1L], reg = reg)
#    bmr_tasks = bmr$tasks
#    bmr_learners = bmr$learners
#    bmr_resamplings = bmr$resamplings
#
#    needle = job$prob.pars$task_hash
#    ii = bmr_tasks[list(needle), on = "task_hash", which = TRUE, nomatch = NULL]
#    if (length(ii)) {
#      task = bmr_tasks$task[[ii]]
#    } else {
#      task = job$problem$data
#    }
#
#    needle = job$prob.pars$resampling_hash
#    ii = bmr_resamplings[list(needle), on = "resampling_hash", which = TRUE, nomatch = NULL]
#    if (length(ii)) {
#      resampling = bmr_resamplings$resampling[[ii]]
#    } else {
#      resampling = get_export(needle, reg)
#    }
#
#    needle = job$algo.pars$learner_hash
#    ii = bmr_learners[list(needle), on = "learner_hash", which = TRUE, nomatch = NULL]
#    if (length(ii)) {
#      learner = bmr_learners$learner[[ii]]
#    } else {
#      learner = get_export(needle, reg)
#    }
#
#    results = batchtools::reduceResultsList(tab$job.id, reg = reg)
#    param_values = lapply(lapply(results, getElement, "learner_state"),
#                          getElement, "param_vals")
#    learner_hash = tab[, learner_hash]
#    #print(test)
#    rdata = mlr3::ResultData$new(data.table(
#      task = list(task),
#      learner = list(learner),
#      resampling = list(resampling),
#      iteration = tab$repl,
#      prediction = map(results, "prediction"),
#      learner_state = map(results, "learner_state"),
#      param_values = param_values,
#      learner_hash = learner_hash,
#      uhash = tab$job.name
#    ), store_backends = store_backends)
#    #print(rdata)
#    bmr$combine(mlr3::BenchmarkResult$new(rdata))
#  }
#
#  return(bmr)
#}

#get_export = function(needle, reg) {
  #readRDS(batchtools::batchExport(reg = reg)[list(needle), on = "name"]$uri)
#}

