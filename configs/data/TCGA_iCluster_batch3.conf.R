cancers = c("LGG", "HNSC", "LUSC")

cv = rsmp("custom")
folds = readRDS("data/folds.rds")
folds = folds[cancers]
paired_cv = TRUE

task_type="survival"

datasets = list()
for (c in cancers) {
    datasets[[c]] = list(
        path=paste0("data/herrmann2021/", c, ".csv"),
        target=c("time", "status"),
        strata=c("status")
    )
}
