suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

source("R/learners/JIVE_surv_learner.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

# rankJ = 0 or rankA=0 not working due to jive.predict
learners <- list(
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=1,
        rankA=1,
        clinical_fav=FALSE,
        seed=internal_seed,
        id="jive_j1_a1"
    ), 
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=3,
        rankA=1,
        clinical_fav=FALSE,
        seed=internal_seed,
        id="jive_j3_a1"
    ), 
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=1,
        rankA=3,
        clinical_fav=FALSE,
        seed=internal_seed,
        id="jive_j1_a3"
    ), 
    lrn(
        "surv.jive",
        blocks=blocks,
        rankJ=3,
        rankA=3,
        clinical_fav=FALSE,
        seed=internal_seed,
        id="jive_j3_a3"
    )
)

resources = list(
    "jive_j1_a1"=list(
        walltime=60*60*24, ncpus=3, memory=10000, jobs_per_chunk=1
    ),
    "jive_j3_a1"=list(
        walltime=60*60*24, ncpus=3, memory=10000, jobs_per_chunk=1
    ),
    "jive_j1_a3"=list(
        walltime=60*60*24, ncpus=3, memory=10000, jobs_per_chunk=1
    ),
    "jive_j3_a3"=list(
        walltime=60*60*24, ncpus=3, memory=10000, jobs_per_chunk=1
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
