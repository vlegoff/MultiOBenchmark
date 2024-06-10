suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

learners <- list(
    lrn(
        "surv.mofa",
        blocks=blocks,
        clinical_fav=TRUE,
        num_factors=3,
        center=TRUE,
        scale_views=TRUE,
        convergence_mode="slow",
        CV_measure="V&VH",
        seed=internal_seed,
        id="mofa_clin_fav_f3_scale"
    ),
    lrn(
        "surv.mofa",
        blocks=blocks,
        clinical_fav=TRUE,
        num_factors=3,
        center=FALSE,
        scale_views=FALSE,
        convergence_mode="slow",
        CV_measure="V&VH",
        seed=internal_seed,
        id="mofa_clin_fav_f3_scale"
    ),
)
