.libPaths(.libPaths()[[2]]) # to ensure use of local libs

suppressMessages({
    library(batchtools)
    library(data.table)
    library(argparse)
})

parser = ArgumentParser()

parser$add_argument("-r", "--registry", nargs=1,
    help="Name of registry to extract results from. The registry must exist")

args = parser$parse_args()

registry_dir = paste0("registries/", args$registry)

# Loading batchtools registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
} else {
    stop("registry must exists")
}

v = function(...) cat(sprintf(...), sep='', file=stdout())
verr = function(...) cat(sprintf(...), sep='', file=stderr())

done = findDone()
not_sub = findNotSubmitted()
not_done = findNotDone()
err = findErrors()

print("Jobs summary:")
print(summarizeExperiments(by=c("learner_id")))
if (nrow(done)>0) {
    print("Jobs done:")
    print(summarizeExperiments(done, by=c("learner_id", "task_id")))
}
if (nrow(not_sub)>0) {
    print("Jobs not submitted:")
    print(summarizeExperiments(not_sub, by=c("learner_id", "task_id")))
}
if (nrow(not_done)>0) {
    print("Jobs not done:")
    print(summarizeExperiments(not_done, by=c("learner_id", "task_id")))
}
if (nrow(err)>0) {
    print("Errors:")
    print(summarizeExperiments(err, by=c("learner_id", "task_id")))
    print("Error messages:")
    print(getErrorMessages())
}
