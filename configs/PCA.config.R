suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

source("R/learners/PCA_joint_surv_learner.R")
source("R/learners/PCA_indiv_surv_learner.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

learners <- list(
    lrn(
        "surv.pca_joint",
        blocks=blocks,
        rankJ=1,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="jive_clin_fav_j1_a0"
    ), 
    lrn(
        "surv.pca_joint",
        blocks=blocks,
        rankJ=3,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="jive_clin_fav_j3_a0"
    ),
    lrn(
        "surv.pca_indiv",
        blocks=blocks,
        rankA=1,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="jive_clin_fav_j0_a1"
    ), 
    lrn(
        "surv.pca_indiv",
        blocks=blocks,
        rankA=3,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="jive_clin_fav_j0_a3"
    )
)

resources = list(
    "jive_clin_fav_j1_a0"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    ),
    "jive_clin_fav_j3_a0"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    ),
    "jive_clin_fav_j0_a1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    ),
    "jive_clin_fav_j0_a3"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    )
)

measures = list(
    msr("surv.cindex", weight_meth="G2"), # uno C
    msr("surv.graf"), # ibrier
    msr("surv.rcll"),
    msr("surv.dcalib"),
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
