suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/MCIA_woClin"
learners <- list()

pred_mode = c("Ntrain", "Ntest")
cia.nf = c(2, 5, 10)
params_comb = expand.grid(pred_mode=pred_mode, cia.nf=cia.nf)
params_comb$pred_mode = as.character(params_comb$pred_mode)
#print(as.factor(params_comb$pred_mode))
#print(params_comb)
print(apply(params_comb, 2, class))
for (i in 1:nrow(params_comb)) {
    print(params_comb[i, "pred_mode"])
    learners <- c(learners,
        lrn(
            "surv.omicade4",
            blocks=blocks,
            clinical_fav=FALSE,
            cia.nf=params_comb[i, "cia.nf"],
            pred_mode=params_comb[i, "pred_mode"],
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("MCIA_", params_comb[i, "pred_mode"],
                      "_r", params_comb[i, "cia.nf"])
        )
    )
}
