suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

source("R/learners/RGCCA_surv_learner.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

learners <- list(
    lrn(
        "surv.rgcca",
        blocks=blocks,
        tau=1,
        clinical_fav=TRUE,
        supervised=FALSE,
        ncomp=1,
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
        seed=internal_seed,
        id="rgcca_sup_clin_fav_n3_tau0.1"
    ) 
)

resources = list(
    "rgcca_unsup_clin_fav_n1_tau1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "rgcca_unsup_clin_fav_n3_tau1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "rgcca_sup_clin_fav_n1_tau1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "rgcca_sup_clin_fav_n3_tau1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "rgcca_unsup_clin_fav_n1_tau0.1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "rgcca_unsup_clin_fav_n3_tau0.1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "rgcca_sup_clin_fav_n1_tau0.1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "rgcca_sup_clin_fav_n3_tau0.1"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
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
