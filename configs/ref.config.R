suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

learners <- list(
    lrn("surv.kaplan", id="kaplan"),
    GraphLearner$new(
        po("select", selector=selector_grep("_clinical$")) %>>%
        lrn("surv.coxph"),
        id="clin_ref",
    ),
    GraphLearner$new(
        po("select", selector=selector_grep("_clinical$")) %>>%
        lrn("surv.cv_glmnet", alpha=0, s="lambda.min", type.measure="C"),
        id="glmnet_clin",
    )
)

resources = list(
    "kaplan"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=100
    ),
    "clin_ref"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=300
    ),
    "glmnet_clin"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=300
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
