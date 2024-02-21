suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(mlr3tuning)
    library(mlr3misc)
    library(data.table)
})

source("R/learners/cv_prioritylasso_learner.R")

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

learners <- list(
    lrn(
        "surv.cv_prioritylasso",
        id="prioritylasso",
        block1.penalization=TRUE,
        lambda.type="lambda.min",
        standardize=TRUE,
        nfolds=5,
        cvoffset=TRUE,
        cvoffsetnfolds=5,
        alpha=0.95,
        clinical_favoring=FALSE
    ),
    lrn(
        "surv.cv_prioritylasso",
        id="prioritylasso_clin_fav",
        block1.penalization=TRUE,
        lambda.type="lambda.min",
        standardize=TRUE,
        nfolds=5,
        cvoffset=TRUE,
        cvoffsetnfolds=5,
        alpha=0.95,
        clinical_favoring=TRUE
    )
)

resources = list(
    "prioritylasso"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=5
    ),
    "prioritylasso_clin_fav"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=5
    )
)

measures = list(
    msr("surv.cindex", weight_meth="G2"), # uno C
    msr("surv.graf"), # ibrier
    msr("surv.rcll"),
    msr("surv.dcalib"),
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
