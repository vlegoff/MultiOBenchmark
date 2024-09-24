suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    #library(mlr3tuning)
    library(mlr3misc)
    library(data.table)
    library(survjdr)
})


learners <- list(
    lrn("surv.ranger", id="ranger", min.node.size=3),
    lrn("surv.cv_glmnet", id="lasso", alpha=1, s="lambda.min",
        type.measure="C"),
    #lrn("surv.cv_coxboost", id="coxboost", maxstepno=100),
    #lrn("surv.cv_coxboost_clin_fav", id="coxboost_clin_fav", maxstepno=100),
    #auto_tuner(
        #tuner=tnr("grid_search", resolution=1, batch_size=1),
        #learner=lrn(
            #"surv.glmboost",
            #mstop=to_tune(1, 100, logscale=FALSE)
        #),
        #resampling=rsmp("cv", folds=5),
        #measure=msr("surv.cindex", weight_meth="G2")
    #),
    lrn(
        "surv.cv_prioritylasso",
        id="prioritylasso_clin",
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
        id="blockforest_clin",
        block.method="BlockForest",
        num.trees=2000,
        nsets=300,
        num.trees.pre=1500
    ),
    lrn(
        "surv.ipflasso",
        id="ipflasso_clin",
        blocks=c("cnv", "mirna", "mutation","rna", "clinical"),
        seed=123
    )
)

#resample(task, lrn("surv.glmboost", mstop=2), rsmp("cv", folds=2))
