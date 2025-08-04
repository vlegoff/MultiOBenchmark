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
