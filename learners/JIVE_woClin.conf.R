suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/JIVE_woClin"

# rankJ = 0 or rankA=0 not working due to jive.predict
learners <- list(
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=1,
        rankA=1,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j1_a1"
    ), 
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=3,
        rankA=1,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j3_a1"
    ), 
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=1,
        rankA=3,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j1_a3"
    ), 
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=3,
        rankA=3,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="jive_j3_a3"
    )
)
