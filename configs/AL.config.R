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
    lrn("surv.ranger", id="ranger", min.node.size=3),
    lrn("surv.cv_glmnet", id="lasso", alpha=1, s="lambda.min",
        type.measure="C"),
    #lrn("surv.cv_coxboost", id="coxboost", maxstepno=100),
    #lrn("surv.cv_coxboost_clin_fav", id="coxboost_clin_fav", maxstepno=100),
    auto_tuner(
        tuner=tnr("grid_search", resolution=100, batch_size=1),
        learner=lrn(
            "surv.glmboost",
            mstop=to_tune(1, 100, logscale=FALSE)
        ),
        resampling=rsmp("cv", folds=5),
        measure=msr("surv.cindex", weight_meth="G2")
    ),
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
    ),
    lrn(
        "surv.blockforest",
        id="blockforest",
        block.method="BlockForest",
        num.trees=2000,
        nsets=300,
        num.trees.pre=1500
    ),
    lrn(
        "surv.ipflasso",
        id="ipflasso",
        blocks=c("cnv", "mirna", "mutation","rna", "clinical"),
        seed=123
    )
)

#resample(task, lrn("surv.glmboost", mstop=2), rsmp("cv", folds=2))

#test_lrn = lrn("surv.ipflasso", id="ipflasso", blocks=c("mirna", "clinical"))
#cv2 = rsmp("cv", folds=2)
#resample(task, test_lrn, cv2)

resources = list(
    "ranger"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=1
    ),
    "lasso"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    ),
    #"coxboost_clin_fav"=list(
        #walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=30
    #),
    #"coxboost"=list(
        #walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=30
    #),
    #"surv.glmboost.tuned"=list(
        #walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    #),
    #"prioritylasso"=list(
        #walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=10
    #),
    #"prioritylasso_clin_fav"=list(
        #walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=10
    #),
    "blockforest"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=1
    ),
    "ipflasso"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=1
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
