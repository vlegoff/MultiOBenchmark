suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/RGCCA_woClin"
learners = list()

tau = c(0.1, 1)
supervised=c(TRUE, FALSE)
ncomp = c(1, 5, 10)
params_comb = expand.grid(tau=tau, supervised=supervised, ncomp=ncomp)
for (i in 1:nrow(params_comb)) {
    learners <- c(learners,
        lrn(
            "surv.rgcca",
            blocks=blocks,
            clinical_fav=FALSE,
            tau=params_comb[i, "tau"],
            supervised=params_comb[i, "supervised"],
            ncomp=params_comb[i, "ncomp"],
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("rgcca_", ifelse(params_comb[i, "supervised"], "sup", "unsup"),
                       "_n", params_comb[i, "ncomp"],
                       "_tau", params_comb[i, "tau"])
        )
    )
}

#learners <- list(
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=1,
#        clinical_fav=FALSE,
#        supervised=FALSE,
#        ncomp=1,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_unsup_n1_tau1"
#    ), 
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=1,
#        clinical_fav=FALSE,
#        supervised=FALSE,
#        ncomp=3,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_unsup_n3_tau1"
#    ), 
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=1,
#        clinical_fav=FALSE,
#        supervised=TRUE,
#        ncomp=1,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_sup_n1_tau1"
#    ), 
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=1,
#        clinical_fav=FALSE,
#        supervised=TRUE,
#        ncomp=3,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_sup_n3_tau1"
#    ), 
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=0.1,
#        clinical_fav=FALSE,
#        supervised=FALSE,
#        ncomp=1,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_unsup_n1_tau0.1"
#    ), 
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=0.1,
#        clinical_fav=FALSE,
#        supervised=FALSE,
#        ncomp=3,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_unsup_n3_tau0.1"
#    ), 
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=0.1,
#        clinical_fav=FALSE,
#        supervised=TRUE,
#        ncomp=1,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_sup_n1_tau0.1"
#    ), 
#    lrn(
#        "surv.rgcca",
#        blocks=blocks,
#        tau=0.1,
#        clinical_fav=FALSE,
#        supervised=TRUE,
#        ncomp=3,
#        CV_measure="V&VH",
#        seed=internal_seed,
#        cv_save_path=cv_save_path,
#        id="rgcca_sup_n3_tau0.1"
#    ) 
#)
