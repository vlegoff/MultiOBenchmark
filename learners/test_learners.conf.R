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
