suppressMessages({
    library(mlr3verse)
    library(mlr3proba)
    library(mlr3extralearners)
    library(data.table)
    library(survjdr)
})

blocks = c("cnv", "mirna", "mutation", "rna")
internal_seed = 123
cv_save_path = "~/scratch_MS/legoff/benchmark/results/cv_logs/PCA"
learners = list()

rankJ = c(1, 5, 10)
params_comb = expand.grid(rankJ=rankJ)
#print(as.factor(params_comb$pred_mode))
for (i in 1:nrow(params_comb)) {
    # standard PCA
    learners <- c(learners,
        lrn(
            "surv.pca_joint",
            blocks=blocks,
            rankJ=params_comb[i, "rankJ"],
            center=TRUE,
            scale=TRUE,
            norm=FALSE,
            clinical_fav=TRUE,
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("jpca_clin_fav_j", params_comb[i, "rankJ"])
        )
    )
    # JIVE like PCA
    learners <- c(learners,
        lrn(
            "surv.pca_joint",
            blocks=blocks,
            rankJ=params_comb[i, "rankJ"],
            center=TRUE,
            scale=FALSE,
            norm=TRUE,
            clinical_fav=TRUE,
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("jive_clin_fav_j", params_comb[i, "rankJ"], "_a0")
        )
    )
}

rankA = c(1, 5, 10)
params_comb = expand.grid(rankA=rankA)
#print(as.factor(params_comb$pred_mode))
for (i in 1:nrow(params_comb)) {
    learners <- c(learners,
        lrn(
            "surv.pca_indiv",
            blocks=blocks,
            rankA=params_comb[i, "rankA"],
            center=TRUE,
            scale=TRUE,
            norm=FALSE,
            clinical_fav=TRUE,
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("ipca_clin_fav_a", params_comb[i, "rankA"])
        )
    )
    learners <- c(learners,
        lrn(
            "surv.pca_indiv",
            blocks=blocks,
            rankA=params_comb[i, "rankA"],
            center=TRUE,
            scale=FALSE,
            norm=TRUE,
            clinical_fav=TRUE,
            CV_measure="V&VH",
            nlambdas=100,
            nfolds=10,
            cv_save_path=cv_save_path,
            seed=internal_seed,
            id=paste0("jive_clin_fav_j0_a", params_comb[i, "rankA"])
        )
    )
}

#learners <- list(
#    lrn(
#        "surv.pca_joint",
#        blocks=blocks,
#        rankJ=1,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j1_a0"
#    ), 
#    lrn(
#        "surv.pca_joint",
#        blocks=blocks,
#        rankJ=3,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j3_a0"
#    ),
#    lrn(
#        "surv.pca_indiv",
#        blocks=blocks,
#        rankA=1,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j0_a1"
#    ), 
#    lrn(
#        "surv.pca_indiv",
#        blocks=blocks,
#        rankA=3,
#        clinical_fav=TRUE,
#        CV_measure="V&VH",
#        cv_save_path=cv_save_path,
#        seed=internal_seed,
#        id="jive_clin_fav_j0_a3"
#    )
#)
