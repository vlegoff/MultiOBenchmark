library(data.table)
library(tidyverse)

clin_registry = c("ref",
    "AL",
    "RGCCA",
    "SGCCA",
    "JIVE",
    "PCA",
    "SJIVE",
    "SJIVE2",
    "intNMF",
    "MOFA",
    "MOFA_hvg")
    #"RGCCA_test_clin",
clin_registry = paste0(clin_registry, "/", clin_registry, "_results.csv")
tbls = lapply(clin_registry, fread)
table_clin = rbindlist(tbls, fill=TRUE)
table_clin[,clin:="with clinical data"]

woclin_registry = c("AL_woClin",
    "RGCCA_woClin",
    "SGCCA_woClin",
    "JIVE_woClin",
    "PCA_woClin",
    "SJIVE_woClin",
    "SJIVE2_woClin",
    "intNMF_woClin",
    "MOFA_woClin",
    "MOFA_hvg_woClin")
woclin_registry = paste0(woclin_registry, "/", woclin_registry, "_results.csv")
tbls = lapply(woclin_registry, fread)
table_woclin = rbindlist(tbls, fill=TRUE)
table_woclin[,clin:="without clinical data"]

bmr = rbind(table_clin, table_woclin)
fwrite(bmr, "results_aggregated_raw.csv")

bmr %<>%
    mutate(full_learner_id=learner_id) %>%
    mutate(learner_id = ifelse(!grepl("prioritylasso", learner_id),
        str_replace(learner_id, "_clin_fav", ""),
        learner_id)) %>%
        complete(clin, task_id, iteration, learner_id) %>%
        filter(!(learner_id %in% c("clin_ref", "glmnet_clin", "kaplan",
                                   "prioritylasso_clin_fav") 
                 & clin=="without clinical data")) %>%
        mutate(type=case_when(
            learner_id %in% c("kaplan", "clin_ref", "glmnet_clin")~"Reference",
            learner_id %in% c("ranger", "blockforest") ~ "Prev. study",
            grepl("lasso", learner_id) ~ "Prev. study",
            grepl("rgcca_sup2", learner_id) ~ "RGCCA Sup. Clin.",
            grepl("rgcca_sup", learner_id) ~ "RGCCA Sup.",
            grepl("rgcca_unsup", learner_id) ~ "RGCCA Unsup.",
            grepl("sgcca_sup", learner_id) ~ "SGCCA Sup.",
            grepl("sgcca_unsup", learner_id) ~ "SGCCA Unsup.",
            grepl("sjive", learner_id) ~ "sJIVE",
            grepl("[ja]0", learner_id) ~ "PCA",
            grepl("jive", learner_id) ~ "JIVE",
            grepl("mofa", learner_id) ~ "MOFA",
            grepl("intnmf", learner_id) ~ "IntNMF"
            )
        )

bmr %>% select(learner_id, type) %>% table()
bmr %>% filter(is.na(type)) %>% pull(learner_id) %>% unique()

fwrite(bmr, "results_aggregated_completed.csv")

measures = source("../measures.conf.R")
measures = sapply(measures$value, `[[`, "id")
cindex = measures[grepl("cindex", measures)]
graf = measures[grepl("graf", measures)]

replace_na = function(x, val, thresh=0.2) {
    na_rate = sum(is.na(x))/length(x)
    if (na_rate<thresh) {
        x[is.na(x)] = mean(x, na.rm=TRUE)
    } else {
        x[is.na(x)] = val
    }
    return(x)
}

bmr_imp = bmr %>%
    group_by(clin, learner_id) %>%
    mutate(across(all_of(cindex), \(x) replace_na(x, .5))) %>%
    mutate(across(all_of(graf), \(x) replace_na(x, .25)))

bmr_imp %>% filter(is.na(cindex_p0.9))
bmr_imp %>% filter(is.na(graf_p0.9))

fwrite(bmr, "results_aggregated_imputed.csv")

# Cross Validation Hack
# Still to check:
# - that iteration and the index in the list of folds are the same
# - better way to check clin/non clin learners

folds = readRDS("../data/folds.rds")
cv_files = list.files("cv_logs", recursive=TRUE, full.names=TRUE)
cv_results = lapply(cv_files, readRDS)
folds_df = list()
for (n in names(folds)) {
    test = sapply(
        folds[[n]]$test,
        paste, collapse=" "
    )    
    folds_df[[n]] = data.frame(task_id=n, 
        iteration=(1:length(test)),
        fold=test)
}
folds_df = Reduce(rbind, folds_df)

cv_df = tibble(
    full_learner_id = sapply(cv_results, `[[`, "learner_id"),
    task_id = sapply(cv_results, `[[`, "task_id"),
    fold = sapply(cv_results, \(x) paste(x$fold, collapse=" ")),
    lambda = sapply(cv_results, `[[`, "best_lambda"),
    cv_metric = sapply(cv_results, \(x) min(x$cv_results))
) %>%
    tibble() %>%
    left_join(folds_df, by=c("task_id", "fold")) %>%
    select(-fold)

bmr_cv = bmr_imp %>%
    left_join(cv_df, by=c("full_learner_id", "task_id", "iteration"))

bmr_cv %>%
    filter(!is.na(cv_metric)) %>%
    select(clin, learner_id, iteration, lambda, cv_metric)
