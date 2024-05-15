suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

cv = rsmp("custom")
folds = readRDS("folds.rds")
paired_cv = TRUE

source("R/learners/learner_sjive_surv.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

# rankJ = 0 or rankA=0 not working due to jive.predict
learners <- list(
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=1,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j1_a1_eta0.5"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=1,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j3_a1_eta0.5"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=3,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j1_a3_eta0.5"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=3,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j3_a3_eta0.5"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=0,
        rankA=1,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j0_a1_eta0.5"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=0,
        rankA=3,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j0_a3_eta0.5"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=0,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j1_a0_eta0.5"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=0,
        eta=0.5,
        clinical_fav=TRUE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sjive_clin_fav_j3_a0_eta0.5"
    ) 
)

resources = list(
    "pegasus" = list(
        walltime=60*60*24, ntasks=21, memory=10000, logs="logs/"
    )
)

measures = list(
    msr("surv.cindex", weight_meth="G2", p_max=0.9), # uno C
    msr("surv.cindex", weight_meth="G2", id="cindex_default"), # uno C
    msr("surv.graf", id="graf_default"),
    msr("surv.graf", p_max=0.9), # ibrier
    msr("surv.rcll"),
    msr("surv.dcalib"),
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
