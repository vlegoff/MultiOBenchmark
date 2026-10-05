suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/JIVE"
learners = list()

rankJ = c(1, 5, 10)
rankA = c(1, 5, 10)
params_comb = expand.grid(rankJ=rankJ, rankA=rankA)
#print(as.factor(params_comb$pred_mode))
for (i in 1:nrow(params_comb)) {
    learners <- c(learners,
        lrn(
            "surv.jive",
            blocks=blocks,
            rankJ=params_comb[i, "rankJ"],
            rankA=params_comb[i, "rankA"],
            clinical_fav=TRUE,
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("jive_clin_fav_j", params_comb[i, "rankJ"],
                      "_a", params_comb[i, "rankA"])
        )
    )
}

# rankJ = 0 or rankA=0 not working due to jive.predict
#learners <- list(
#    lrn(
#        "surv.jive",
#        blocks=blocks,
#        rankJ=1,
#        rankA=1,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j1_a1"
#    ), 
#    lrn(
#        "surv.jive",
#        blocks=blocks,
#        rankJ=3,
#        rankA=1,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j3_a1"
#    ), 
#    lrn(
#        "surv.jive",
#        blocks=blocks,
#        rankJ=1,
#        rankA=3,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j1_a3"
#    ), 
#    lrn(
#        "surv.jive",
#        blocks=blocks,
#        rankJ=3,
#        rankA=3,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j3_a3"
#    )
#)
