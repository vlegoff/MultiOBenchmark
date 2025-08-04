suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/PCA_woClin"

learners <- list(
    lrn(
        "surv.pca_joint",
        blocks=blocks,
        rankJ=1,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j1_a0"
    ), 
    lrn(
        "surv.pca_joint",
        blocks=blocks,
        rankJ=3,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j3_a0"
    ),
    lrn(
        "surv.pca_indiv",
        blocks=blocks,
        rankA=1,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j0_a1"
    ), 
    lrn(
        "surv.pca_indiv",
        blocks=blocks,
        rankA=3,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j0_a3"
    )
)
