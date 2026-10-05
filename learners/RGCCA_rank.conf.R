suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/RGCCA_rank"

ncomps = c(1, 5, 10, 20, 30)
learners = list()

for (n in ncomps) {
    learners = c(learners,
        lrn(
            "surv.rgcca",
            blocks=blocks,
            tau=1,
            clinical_fav=FALSE,
            supervised=FALSE,
            ncomp=n,
            CV_measure="V&VH",
            seed=internal_seed,
            cv_save_path=cv_save_path,
            init="random",
            id=paste0("rgcca_unsup_rank_n_", n, "_tau1")
        ))
}
