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

source("R/learners/learner_pca_joint_surv.R")
source("R/learners/learner_pca_indiv_surv.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

learners <- list(
    lrn(
        "surv.pca_joint",
        blocks=blocks,
        rankJ=1,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="jive_j1_a0"
    ), 
    lrn(
        "surv.pca_joint",
        blocks=blocks,
        rankJ=3,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="jive_j3_a0"
    ),
    lrn(
        "surv.pca_indiv",
        blocks=blocks,
        rankA=1,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="jive_j0_a1"
    ), 
    lrn(
        "surv.pca_indiv",
        blocks=blocks,
        rankA=3,
        clinical_fav=FALSE,
        CV_measure="V&VH",
        seed=internal_seed,
        id="jive_j0_a3"
    )
)

resources = list(
    "pegasus" = list(
        walltime=60*60*24, ntasks=100, memory=10000, logs="logs/"
    ),
    "jive_j1_a0"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "jive_j3_a0"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "jive_j0_a1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "jive_j0_a3"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
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
