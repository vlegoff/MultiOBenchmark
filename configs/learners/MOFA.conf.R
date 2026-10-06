suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/MOFA"

learners <- list(
    lrn(
        "surv.mofa",
        blocks=blocks,
        clinical_fav=TRUE,
        num_factors=2,
        center=TRUE,
        scale_views=TRUE,
        spikeslab_factors=FALSE,
        spikeslab_weights=TRUE,
        ard_factors=FALSE,
        ard_weights=TRUE,
        convergence_mode="slow",
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="mofa_clin_fav_f2"
    ),
    lrn(
        "surv.mofa",
        blocks=blocks,
        clinical_fav=TRUE,
        num_factors=5,
        center=TRUE,
        scale_views=TRUE,
        spikeslab_factors=FALSE,
        spikeslab_weights=TRUE,
        ard_factors=FALSE,
        ard_weights=TRUE,
        convergence_mode="slow",
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="mofa_clin_fav_f5"
    ),
    lrn(
        "surv.mofa",
        blocks=blocks,
        clinical_fav=TRUE,
        num_factors=10,
        center=TRUE,
        scale_views=TRUE,
        spikeslab_factors=FALSE,
        spikeslab_weights=TRUE,
        ard_factors=FALSE,
        ard_weights=TRUE,
        convergence_mode="slow",
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="mofa_clin_fav_f10"
    )
)
