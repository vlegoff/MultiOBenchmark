cv = rsmp("cv", folds=5)

task_type="survival"

datasets = list(
    "COAD"=list(
        path="data/herrmann2021/COAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "KIRC"=list(
        path="data/herrmann2021/KIRC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LUSC"=list(
        path="data/herrmann2021/LUSC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "OV"=list(
        path="data/herrmann2021/OV.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "SKCM"=list(
        path="data/herrmann2021/SKCM.csv",
        target=c("time", "status"),
        strata=c("status")
    )
)
