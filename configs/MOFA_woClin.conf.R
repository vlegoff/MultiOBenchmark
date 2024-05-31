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

source("R/learners/learner_mofa_surv.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

learners <- list(
    lrn(
        "surv.mofa",
        blocks=blocks,
        clinical_fav=FALSE,
        num_factors=3,
        convergence_mode="slow",
        CV_measure="V&VH",
        seed=internal_seed,
        id="mofa_f3"
    )
)

resources = list(
    "pegasus" = list(
        walltime=60*60*24, ntasks=172, memory=10000, logs="logs/"
    )
)

measures = list(
    msr("surv.cindex", weight_meth="G2"), # uno C
    msr("surv.graf", id="graf_default"),
    msr("surv.graf", p_max=0.1), # ibrier
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
