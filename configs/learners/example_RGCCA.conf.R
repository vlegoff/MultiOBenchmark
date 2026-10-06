suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

# running a fast supervised RGCCA

blocks = c("mirna", "rna")
internal_seed = 123
cv_save_path = "results/cv_logs/example_RGCCA"
learners = list()

tau = c(1)
supervised=c(TRUE)
ncomp = c(1, 2)
params_comb = expand.grid(tau=tau, supervised=supervised, ncomp=ncomp)
for (i in 1:nrow(params_comb)) {
    learners <- c(learners,
        lrn(
            "surv.rgcca",
            blocks=blocks,
            clinical_fav=TRUE,
            tau=params_comb[i, "tau"],
            supervised=params_comb[i, "supervised"],
            ncomp=params_comb[i, "ncomp"],
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=3,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("rgcca_", ifelse(params_comb[i, "supervised"], "sup", "unsup"),
                       "_clin_fav_n", params_comb[i, "ncomp"],
                       "_tau", params_comb[i, "tau"])
        )
    )
}
