suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/iCluster_batch3"
learners = list()

s = c(0, 0.33, 0.66)
K = c(2, 6)
params_comb = expand.grid(s=s, K=K)
#print(as.factor(params_comb$pred_mode))
for (i in 1:nrow(params_comb)) {
    print(params_comb[i, "pred_mode"])
    learners <- c(learners,
        lrn(
            "surv.icluster2",
            blocks=blocks,
            clinical_fav=TRUE,
            K=params_comb[i, "K"],
            s=params_comb[i, "s"],
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=3,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            maxiter=20000,
            id=paste0("icluster_", "_clin_fav_K", params_comb[i, "K"],
                       "_s", params_comb[i, "s"])
        )
    )
    learners <- c(learners,
        lrn(
            "surv.icluster2",
            blocks=blocks,
            clinical_fav=FALSE,
            K=params_comb[i, "K"],
            s=params_comb[i, "s"],
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=3,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            maxiter=20000,
            id=paste0("icluster_", "_K", params_comb[i, "K"],
                       "_s", params_comb[i, "s"])
        )
    )
}
