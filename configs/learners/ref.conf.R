suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

learners <- list(
    lrn("surv.kaplan", id="kaplan_ref"),
    GraphLearner$new(
        po("select", selector=selector_grep("_clinical$")) %>>%
        lrn("surv.coxph"),
        id="cox_ref",
    ),
    GraphLearner$new(
        po("select", selector=selector_grep("_clinical$")) %>>%
        lrn("surv.cv_glmnet", alpha=0, s="lambda.min", type.measure="C"),
        id="glmnet_ref",
    )
)
