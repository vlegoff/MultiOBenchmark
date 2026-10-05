suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/tFOBI_woClin"
learners <- list()

ranksPCA = c(2, 5, 10)
tICA = c("tFOBI")
params_comb = expand.grid(ranksPCA=ranksPCA, tICA=tICA)
params_comb$tICA = as.character(params_comb$tICA)
for (i in 1:nrow(params_comb)) {
    learners <- c(
        lrn(
            "surv.tFOBI",
            blocks=blocks,
            clinical_fav=FALSE,
            rankPCA=params_comb[i, "ranksPCA"],
            tICA=params_comb[i, "tICA"],
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("tfobi_r", params_comb[i, "ranksPCA"])
        )
    )
}
