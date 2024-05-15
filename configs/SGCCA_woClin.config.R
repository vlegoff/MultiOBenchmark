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

#source("R/learners/RGCCA_surv_learner.R")
source("R/learners/learner_sgcca_surv.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

learners <- list(
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.5,
        clinical_fav=FALSE,
        supervised=FALSE,
        ncomp=1,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_unsup_n1_s0.5"
    ), 
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.5,
        clinical_fav=FALSE,
        supervised=FALSE,
        ncomp=3,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_unsup_n3_s0.5"
    ), 
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.5,
        clinical_fav=FALSE,
        supervised=TRUE,
        ncomp=1,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_sup_n1_s0.5"
    ), 
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.5,
        clinical_fav=FALSE,
        supervised=TRUE,
        ncomp=3,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_sup_n3_s0.5"
    ), 
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.75,
        clinical_fav=FALSE,
        supervised=FALSE,
        ncomp=1,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_unsup_n1_s0.75"
    ), 
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.75,
        clinical_fav=FALSE,
        supervised=FALSE,
        ncomp=3,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_unsup_n3_s0.75"
    ), 
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.75,
        clinical_fav=FALSE,
        supervised=TRUE,
        ncomp=1,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_sup_n1_s0.75"
    ), 
    lrn(
        "surv.sgcca",
        blocks=blocks,
        sparsity=0.75,
        clinical_fav=FALSE,
        supervised=TRUE,
        ncomp=3,
        CV_measure="V&VH",
        seed=internal_seed,
        id="sgcca_sup_n3_s0.75"
    ) 
)

resources = list(
    "pegasus" = list(
        walltime=60*60*24, ntasks=200, memory=10000, logs="logs/"
    ),
    "sgcca_unsup_n1_s0.5"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "sgcca_unsup_n3_s0.5"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "sgcca_sup_n1_s0.5"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "sgcca_sup_n3_s0.5"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "sgcca_unsup_n1_s0.75"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "sgcca_unsup_n3_s0.75"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "sgcca_sup_n1_s0.75"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    ),
    "sgcca_sup_n3_s0.75"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=20
    )
)

measures = list(
    msr("surv.cindex", weight_meth="G2"), # uno C
    msr("surv.graf", id="graf_default"),
    msr("surv.graf", p_max=0.1), # ibrier
    msr("surv.rcll"),
    msr("surv.dcalib"),
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
