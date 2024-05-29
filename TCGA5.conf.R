cv = rsmp("cv", folds=5)

task_type="survival"

datasets = list(
    "KIRC"=list(
        path="data/KIRC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "KIRP"=list(
        path="data/KIRP.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LUSC"=list(
        path="data/LUSC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "OV"=list(
        path="data/OV.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "SKCM"=list(
        path="data/SKCM.csv",
        target=c("time", "status"),
        strata=c("status")
    )
)
