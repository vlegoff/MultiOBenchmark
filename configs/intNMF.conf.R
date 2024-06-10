suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

# rankJ = 0 or rankA=0 not working due to jive.predict
learners <- list(
    lrn(
        "surv.intnmf",
        blocks=blocks,
        k=2,
        weights="frob",
        clinical_fav=TRUE,
        n.ini=10, # faster and should not impact that much
        CV_measure="V&VH",
        seed=internal_seed,
        id="intnmf_clin_fav_k2_frob"
    ), 
    lrn(
        "surv.intnmf",
        blocks=blocks,
        k=3,
        weights="frob",
        clinical_fav=TRUE,
        n.ini=10,
        CV_measure="V&VH",
        seed=internal_seed,
        id="intnmf_clin_fav_k3_frob"
    ), 
    lrn(
        "surv.intnmf",
        blocks=blocks,
        k=5,
        weights="frob",
        clinical_fav=TRUE,
        n.ini=10,
        CV_measure="V&VH",
        seed=internal_seed,
        id="intnmf_clin_fav_k5_frob"
    ) 
)
