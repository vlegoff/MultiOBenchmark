suppressMessages({
    library(benchbatch)
    library(batchtools)
    library(data.table)
    library(argparse)
    library(parallel)
    library(tidyverse)
})

parser = ArgumentParser()
parser$add_argument("-f", "--folder", nargs=1,
    help="Folder storing cv results as rds file, one for each learner")
parser$add_argument("-c", "--folds", nargs=1,
    help="rds file storing the folds used to cross-validate")
parser$add_argument("-N", "--Ncpus", default=1,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-d", "--directory", default="results",
    help="where to store results")
parser$add_argument("-v", "--verbose", action="store_true",
    help="")
args = parser$parse_args()

data.table::setDTthreads(args$Ncpus)

folds = readRDS(args$folds)
#cv_files = list.files("cv_logs", recursive=TRUE, full.names=TRUE,
    #pattern="*.rds")
folds_df = list()
for (n in names(folds)) {
    test = sapply(
        folds[[n]]$test,
        paste, collapse=" "
    )    
    folds_df[[n]] = data.frame(task_id=n, 
        iteration=(1:length(test)),
        fold=test)
}
folds_df = Reduce(rbind, folds_df)

cv_files = list.files(args$folder, recursive=TRUE, full.names=TRUE,
    patter="*.rds")
cv_results = mclapply(cv_files, readRDS, mc.cores=args$Ncpus)

coef_df = rbindlist(lapply(cv_results, \(x) data.table(
    learner_id = x$learner_id,
    task_id = x$task_id,
    fold=paste(x$fold, collapse=" "),
    beta=x$stanbeta,
    comp=names(x$stanbeta)))) %>%
    left_join(folds_df, by=c("task_id", "fold")) %>%
    select(-fold)

cv_df = tibble(
        learner_id = sapply(cv_results, `[[`, "learner_id"),
        task_id = sapply(cv_results, `[[`, "task_id"),
        fold = sapply(cv_results, \(x) paste(x$fold, collapse=" ")),
        lambda = sapply(cv_results, `[[`, "best_lambda"),
        cv_metric = sapply(cv_results, \(x) min(x$cv_results))
    ) %>%
    tibble() %>%
    left_join(folds_df, by=c("task_id", "fold")) %>%
    select(-fold)

res_dir = args$directory
fwrite(cv_df, file.path(res_dir, "raw_cv_results.csv"))
fwrite(coef_df, file.path(res_dir, "raw_coef_results.csv"))
