suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/SGCCA"
learners = list()

sparsity = c(0.25, 0.5, 0.75)
supervised=c(TRUE, FALSE)
ncomp = c(1, 5, 10)
params_comb = expand.grid(sparsity=sparsity, supervised=supervised, ncomp=ncomp)
#print(as.factor(params_comb$pred_mode))
print(params_comb)
print(apply(params_comb, 2, class))
for (i in 1:nrow(params_comb)) {
    print(params_comb[i, "pred_mode"])
    learners <- c(learners,
        lrn(
            "surv.sgcca",
            blocks=blocks,
            clinical_fav=TRUE,
            sparsity=params_comb[i, "sparsity"],
            supervised=params_comb[i, "supervised"],
            ncomp=params_comb[i, "ncomp"],
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("sgcca_", ifelse(params_comb[i, "supervised"], "sup", "unsup"),
                       "_clin_fav_n", params_comb[i, "ncomp"],
                       "_s", params_comb[i, "sparsity"])
        )
    )
}

#learners <- list(
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.5,
#        clinical_fav=TRUE,
#        supervised=FALSE,
#        ncomp=1,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_unsup_clin_fav_n1_s0.5"
#    ), 
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.5,
#        clinical_fav=TRUE,
#        supervised=FALSE,
#        ncomp=3,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_unsup_clin_fav_n3_s0.5"
#    ), 
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.5,
#        clinical_fav=TRUE,
#        supervised=TRUE,
#        ncomp=1,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_sup_clin_fav_n1_s0.5"
#    ), 
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.5,
#        clinical_fav=TRUE,
#        supervised=TRUE,
#        ncomp=3,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_sup_clin_fav_n3_s0.5"
#    ), 
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.75,
#        clinical_fav=TRUE,
#        supervised=FALSE,
#        ncomp=1,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_unsup_clin_fav_n1_s0.75"
#    ), 
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.75,
#        clinical_fav=TRUE,
#        supervised=FALSE,
#        ncomp=3,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_unsup_clin_fav_n3_s0.75"
#    ), 
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.75,
#        clinical_fav=TRUE,
#        supervised=TRUE,
#        ncomp=1,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_sup_clin_fav_n1_s0.75"
#    ), 
#    lrn(
#        "surv.sgcca",
#        blocks=blocks,
#        sparsity=0.75,
#        clinical_fav=TRUE,
#        supervised=TRUE,
#        ncomp=3,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="sgcca_sup_clin_fav_n3_s0.75"
#    ) 
#)
