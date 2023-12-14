suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

source("R/learners/cv_prioritylasso_learner.R")
source("R/learners/blockforest_learner.R")

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

learners <- list(
    lrn("surv.ranger", id="ranger", min.node.size=3),
    lrn("surv.cv_glmnet", id="lasso", alpha=1, s="lambda.min",
        type.measure="C"),
    lrn("surv.cv_coxboost", id="coxboost", maxstepno=100),
    lrn("surv.glmboost", id="glmboost"), # use mlr3tuning for mstop
    lrn(
        "surv.cv_prioritylasso",
        id="prioritylasso"
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
        id="prioritylasso"
        block1.penalization=TRUE,
        lambda.type="lambda.min",
        standardize=TRUE,
        nfolds=5,
        cvoffset=TRUE,
        cvoffsetnfolds=5,
        alpha=0.95,
        clinical_favoring=TRUE
    ) # add blockforest
)

resources = list(
    "kaplan"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    ),
    "clin_ref"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    ),
    "glmnet_clin"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    )
)

measures = list(
    msr("surv.cindex", weight_meth="G2"), # uno C
    msr("surv.graf"), # ibrier
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
