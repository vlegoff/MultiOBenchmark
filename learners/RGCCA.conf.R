suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/RGCCA"
learners = list()

tau = c(0.1, 1)
supervised=c(TRUE, FALSE)
ncomp = c(1, 5, 10)
params_comb = expand.grid(tau=tau, supervised=supervised, ncomp=ncomp)
#print(as.factor(params_comb$pred_mode))
print(params_comb)
print(apply(params_comb, 2, class))
for (i in 1:nrow(params_comb)) {
    print(params_comb[i, "pred_mode"])
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
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("rgcca_", ifelse(params_comb[i, "supervised"], "sup", "unsup"),
                       "_clin_fav_n", params_comb[i, "ncomp"],
                       "_tau", params_comb[i, "tau"])
        )
    )
}

#learners <- list(
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=1,
        #clinical_fav=TRUE,
        #supervised=FALSE,
        #ncomp=1,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_unsup_clin_fav_n1_tau1"
    #), 
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=1,
        #clinical_fav=TRUE,
        #supervised=FALSE,
        #ncomp=3,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_unsup_clin_fav_n3_tau1"
    #), 
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=1,
        #clinical_fav=TRUE,
        #supervised=TRUE,
        #ncomp=1,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_sup_clin_fav_n1_tau1"
    #), 
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=1,
        #clinical_fav=TRUE,
        #supervised=TRUE,
        #ncomp=3,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_sup_clin_fav_n3_tau1"
    #), 
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=0.1,
        #clinical_fav=TRUE,
        #supervised=FALSE,
        #ncomp=1,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_unsup_clin_fav_n1_tau0.1"
    #), 
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=0.1,
        #clinical_fav=TRUE,
        #supervised=FALSE,
        #ncomp=3,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_unsup_clin_fav_n3_tau0.1"
    #), 
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=0.1,
        #clinical_fav=TRUE,
        #supervised=TRUE,
        #ncomp=1,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_sup_clin_fav_n1_tau0.1"
    #), 
    #lrn(
        #"surv.rgcca",
        #blocks=blocks,
        #tau=0.1,
        #clinical_fav=TRUE,
        #supervised=TRUE,
        #ncomp=3,
        #CV_measure="V&VH",
        #cv_save_path=cv_save_path,
        #seed=internal_seed,
        #id="rgcca_sup_clin_fav_n3_tau0.1"
    #) 
#)
