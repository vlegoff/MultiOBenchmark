suppressMessages({
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

registry_dir = paste0("registries/", args$registry)

# Loading batchtools registry
if (dir.exists(registry_dir)) {
    reg <- loadRegistry(registry_dir, writeable=T)
} else {
    stop("registry must exists")
}

# not used anymore, never worked properly
v = function(...) cat(sprintf(...), sep='', file=stdout())
verr = function(...) cat(sprintf(...), sep='', file=stderr())

done = findDone()
not_sub = findNotSubmitted()
err = findErrors()
not_done = findNotDone()[!job.id %in% err$job.id]

summ = summarizeExperiments(by=c("learner_id"))
tot = sum(summ$.count)
print("Jobs summary:")
print(summ)
print(paste0("total: ", tot))
if (nrow(done)>0) {
    by = if(nrow(done) < 100) c("learner_id", "task_id") else "learner_id"
    summ = summarizeExperiments(done, by=by)
    tot = sum(summ$.count)
    print("Jobs done:")
    print(summ)
    print(paste0("total: ", tot))
}
if (nrow(not_sub)>0) {
    by = if(nrow(not_sub) < 100) c("learner_id", "task_id") else "learner_id"
    summ = summarizeExperiments(not_sub, by=by)
    tot = sum(summ$.count)
    print("Jobs not submitted:")
    print(summ)
    print(paste0("total: ", tot))
}
if (nrow(not_done)>0) {
    by = if(nrow(not_done) < 100) c("learner_id", "task_id") else "learner_id"
    summ = summarizeExperiments(not_done, by=by)
    tot = sum(summ$.count)
    print("Jobs not done:")
    print(summ)
    print(paste0("total: ", tot))
}
if (nrow(err)>0) {
    summ = summarizeExperiments(err, by=c("learner_id"))
    tot = sum(summ$.count)
    print("Errors")
    print(summ)
    print(paste0("total: ", tot))
    if (args$message) {
        print("Error messages:")
        print(getErrorMessages())
    }
}
