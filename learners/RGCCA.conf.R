suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

cv = rsmp("custom")
folds = readRDS("data/folds.rds")
paired_cv = TRUE

#source("R/learners/RGCCA_surv_learner.R")
#source("R/learners/learner_rgcca_surv.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/RGCCA"

learners <- list(
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=1,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n1_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=3,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n3_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=1,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n1_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=3,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n3_tau1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=0.1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=1,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n1_tau0.1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=0.1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=3,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_unsup_clin_fav_n3_tau0.1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=0.1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=1,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n1_tau0.1"
    ), 
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=0.1,
        clinical_fav=TRUE,
        supervised=TRUE,
        ncomp=3,
        CV_measure="V&VH",
        cv_save_path=cv_save_path,
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n3_tau0.1"
    ) 
)
