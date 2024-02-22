suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
})

datasets <- c("BLCA", "BRCA", "COAD", "ESCA", "HNSC", "KIRC", "KIRP", "LAML",
              "LGG", "LIHC", "LUAD", "LUSC", "OV", "PAAD", "SARC", "SKCM",
              "STAD", "UCEC")

source("R/learners/sJIVE_surv_learner.R")

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123

# rankJ = 0 or rankA=0 not working due to jive.predict
learners <- list(
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=1,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j1_a1_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=1,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j1_a1_eta0.8"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=1,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j3_a1_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=1,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j3_a1_eta0.8"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=3,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j1_a3_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=3,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j1_a3_eta0.8"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=3,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j3_a3_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=1,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j3_a3_eta0.8"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=0,
        rankA=1,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j0_a1_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=0,
        rankA=1,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j0_a1_eta0.8"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=0,
        rankA=3,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j0_a3_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=0,
        rankA=3,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j0_a3_eta0.8"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=0,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j1_a0_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=1,
        rankA=0,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j1_a0_eta0.8"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=0,
        eta=0.2,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j3_a0_eta0.2"
    ), 
    lrn(
        "surv.sjive",
        blocks=blocks,
        rankJ=3,
        rankA=0,
        eta=0.8,
        clinical_fav=TRUE,
        seed=internal_seed,
        id="sjive_clin_fav_j3_a0_eta0.8"
    ) 
)

resources = list(
    "sjive_clin_fav_j1_a1_eta0.2"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j1_a1_eta0.8"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j3_a1_eta0.2"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j3_a1_eta0.8"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j1_a3_eta0.2"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j1_a3_eta0.8"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j3_a3_eta0.2"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j3_a3_eta0.8"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=50
    )
    "sjive_clin_fav_j0_a1_eta0.2"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=450
    )
    "sjive_clin_fav_j0_a1_eta0.8"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=450
    )
    "sjive_clin_fav_j0_a3_eta0.2"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=450
    )
    "sjive_clin_fav_j0_a3_eta0.8"=list(
        walltime=60*60*24, ncpus=2, memory=10000, jobs_per_chunk=450
    )
)

measures = list(
    msr("surv.cindex", weight_meth="G2"), # uno C
    msr("surv.graf"), # ibrier
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
