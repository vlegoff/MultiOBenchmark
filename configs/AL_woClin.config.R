#.libPaths(.libPaths()[[2]])
.libPaths("/env/export/v_home/q_unix/vlegoff/.local/lib/R/x86_64-el7-linux-gnu-library/4.1") # to ensure use of local libs

suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(mlr3tuning)
    library(mlr3misc)
    library(data.table)
})

source("R/learners/cv_prioritylasso_learner.R")
source("R/learners/blockforest_learner.R")
#source("R/learners/CoxBoost_clin_fav_surv_learner.R")
source("R/learners/IPFlasso_surv_learner.R")
source("R/learners/glmboost_surv_learner.R")

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

learners <- list(
    GraphLearner$new(
        po("select", selector=selector_invert(selector_grep("_clinical$"))) %>>%
        lrn("surv.ranger", min.node.size=3),
        id="ranger"
    ),
    GraphLearner$new(
        po("select", selector=selector_invert(selector_grep("_clinical$"))) %>>%
        lrn("surv.cv_glmnet", alpha=1, s="lambda.min",
            type.measure="C"),
        id="lasso"
    ),
    GraphLearner$new(
        po("select", selector=selector_invert(selector_grep("_clinical$"))) %>>%
        lrn(
            "surv.cv_prioritylasso",
            block1.penalization=TRUE,
            lambda.type="lambda.min",
            standardize=TRUE,
            nfolds=5,
            cvoffset=TRUE,
            cvoffsetnfolds=5,
            alpha=0.95,
            clinical_favoring=FALSE
        ),
        id="prioritylasso"
    ),
    GraphLearner$new(
        po("select", selector=selector_invert(selector_grep("_clinical$"))) %>>%
        lrn(
            "surv.blockforest",
            block.method="BlockForest",
            num.trees=2000,
            nsets=300,
            num.trees.pre=1500
        ),
        id="blockforest"
    ),
    GraphLearner$new(
        po("select", selector=selector_invert(selector_grep("_clinical$"))) %>>%
        lrn(
            "surv.ipflasso",
            blocks=c("cnv", "mirna", "mutation","rna"),
            seed=123
        ),
        id="ipflasso"
    )
)

#resample(task, lrn("surv.glmboost", mstop=2), rsmp("cv", folds=2))

#test_lrn = lrn("surv.ipflasso", id="ipflasso", blocks=c("mirna", "clinical"))
#cv2 = rsmp("cv", folds=2)
#resample(task, test_lrn, cv2)

resources = list(
    "ranger"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=10
    ),
    "lasso"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=10
    ),
    "prioritylasso"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=5
    ),
    "blockforest"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=1
    ),
    "ipflasso"=list(
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
