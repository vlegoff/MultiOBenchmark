suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/intNMF_woClin"

learners <- list(
    lrn(
        "surv.intnmf",
        blocks=blocks,
        k=2,
        weights="frob",
        clinical_fav=FALSE,
        n.ini=10,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="intnmf_k2_frob"
    ), 
    lrn(
        "surv.intnmf",
        blocks=blocks,
        k=3,
        weights="frob",
        clinical_fav=FALSE,
        n.ini=10,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="intnmf_k3_frob"
    ), 
    lrn(
        "surv.intnmf",
        blocks=blocks,
        k=5,
        weights="frob",
        clinical_fav=FALSE,
        n.ini=10,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="intnmf_k5_frob"
    ) 
)
