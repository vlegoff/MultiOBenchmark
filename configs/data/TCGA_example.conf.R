cv = rsmp("cv", folds=5)

task_type="survival"

datasets = list(
    "COAD"=list(
        path="data/herrmann2021/COAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "KIRP"=list(
        path="data/herrmann2021/KIRP.csv",
        target=c("time", "status"),
        strata=c("status")
    )
)
