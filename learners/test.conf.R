suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

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
    ),
    lrn(
        "surv.rgcca",
        blocks=c("rna", "mirna"),
        tau=1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=1,
        CV_measure="V&VH",
        seed=123,
        id="rgcca_unsup_clin_fav_n1_tau1"
    )
)
