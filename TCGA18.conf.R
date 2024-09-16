cv = rsmp("custom")
folds = readRDS("data/folds.rds")
paired_cv = TRUE

task_type="survival"

datasets = list(
    "BLCA"=list( #ID of the dataset
        path="data/BLCA.csv", #path to the data (csv)
        target=c("time", "status"), #column names of the targets
        strata=c("status") #to stratify CV, not obligatory
    ),
    "BRCA"=list(
        path="data/BRCA.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "COAD"=list(
        path="data/COAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "ESCA"=list(
        path="data/ESCA.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "HNSC"=list(
        path="data/HNSC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
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
    "LAML"=list(
        path="data/LAML.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LGG"=list(
        path="data/LGG.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LIHC"=list(
        path="data/LIHC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "LUAD"=list(
        path="data/LUAD.csv",
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
    "PAAD"=list(
        path="data/PAAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "SARC"=list(
        path="data/SARC.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "SKCM"=list(
        path="data/SKCM.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "STAD"=list(
        path="data/STAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "UCEC"=list(
        path="data/UCEC.csv",
        target=c("time", "status"),
        strata=c("status")
    )
)
