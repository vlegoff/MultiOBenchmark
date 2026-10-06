cv = rsmp("custom")
folds = readRDS("data/folds.rds")
paired_cv = TRUE

task_type="survival"

datasets = list(
    "BLCA"=list( #ID of the data/herrmann2021set
        path="data/herrmann2021/BLCA.csv", #path to the data (csv)
        target=c("time", "status"), #column names of the targets
        strata=c("status") #to stratify CV, not obligatory
    ),
    "BRCA"=list(
        path="data/herrmann2021/BRCA.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "COAD"=list(
        path="data/herrmann2021/COAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "ESCA"=list(
        path="data/herrmann2021/ESCA.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "HNSC"=list(
        path="data/herrmann2021/HNSC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "KIRC"=list(
        path="data/herrmann2021/KIRC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "KIRP"=list(
        path="data/herrmann2021/KIRP.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LAML"=list(
        path="data/herrmann2021/LAML.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LGG"=list(
        path="data/herrmann2021/LGG.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LIHC"=list(
        path="data/herrmann2021/LIHC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LUAD"=list(
        path="data/herrmann2021/LUAD.csv",
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
    "PAAD"=list(
        path="data/herrmann2021/PAAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "SARC"=list(
        path="data/herrmann2021/SARC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "SKCM"=list(
        path="data/herrmann2021/SKCM.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "STAD"=list(
        path="data/herrmann2021/STAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "UCEC"=list(
        path="data/herrmann2021/UCEC.csv",
        target=c("time", "status"),
        strata=c("status")
    )
)
