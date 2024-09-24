library(data.table)
library(tidyverse)

clin_registry = c("ref",
    "AL",
    "RGCCA",
    "RGCCA_test_clin",
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
    mutate(across(all_of(c(cindex, graf)), \(x) ifelse(x>1 | x<0, NA, x))) %>%
    mutate(across(all_of(cindex), \(x) replace_na(x, .5))) %>%
    mutate(across(all_of(graf), \(x) replace_na(x, .25))) %>%
    ungroup()

bmr_imp %>% filter(is.na(cindex_p0.9))
bmr_imp %>% filter(is.na(graf_p0.9))

fwrite(bmr_imp, "results_aggregated_imputed.csv")

bmr_for_test = bmr_imp %>%
    filter(type!="Reference") %>%
    pivot_longer(all_of(c(cindex, graf)),
        names_to="measure", values_to="value") %>%
    select(clin, task_id, iteration, learner_id, measure, value) %>%
    left_join(bmr_imp %>%
        filter(learner_id=="clin_ref") %>%
        select(task_id, iteration, all_of(c(cindex, graf))) %>%
        pivot_longer(all_of(c(cindex, graf)), names_to="measure",
            values_to="value_ref")) %>%
    filter(measure %in% c("cindex_p0.9", "graf_p0.9")) %>%
    mutate(measure=ifelse(measure=="cindex_p0.9", "cindex", "graf")) %>%
    ungroup()

fwrite(bmr_for_test, "results_for_testing.csv")

pval_by_learner = bmr_for_test %>%
    group_by(clin, learner_id, measure) %>%
    summarise(
        data.frame(t.test(value,
            value_ref, 
            paired=TRUE)[c("statistic", "p.value")])
    ) %>%
    ungroup() %>%
    mutate(p.adj=p.adjust(p.value, method="BH"))

fwrite(pval_by_learner, "pval_by_learner.csv")

pval_by_cancer = bmr_for_test %>%
    group_by(clin, task_id, learner_id, measure) %>%
    summarise(
        data.frame(t.test(value,
            value_ref, 
            paired=TRUE)[c("statistic", "p.value")])
    ) %>%
    ungroup() %>%
    mutate(p.adj=p.adjust(p.value, method="BH"))

fwrite(pval_by_cancer, "pval_by_cancer.csv")

bmr_perf = bmr_imp %>%
    group_by(clin, task_id, learner_id) %>%
    summarise(across(all_of(measures), mean)) %>%
    select(clin, task_id, learner_id, cindex_p0.9, graf_p0.9) %>%
    rename(cindex=cindex_p0.9, graf=graf_p0.9) %>%
    pivot_longer(c(cindex, graf), names_to="measure", values_to="value")

fwrite(bmr_perf, "results_performance.csv")

bmr_times = bmr_imp %>%
    select(clin, task_id, iteration, learner_id, time_train, time_predict,
        time_both) %>%
    group_by(clin, task_id, learner_id) %>%
    summarize(across(starts_with("time"), \(x) mean(x, na.rm=T))) %>%
    pivot_longer(starts_with("time"), names_to="type", values_to="time",
        names_prefix="time_")

fwrite(bmr_times, "results_times.csv")

pval_by_cancer %>%
    ggplot(aes(sample=p.value)) +
    geom_qq() +
    geom_qq_line()

bmr_imp %>%
    filter(type!="Reference") %>%
    pivot_longer(all_of(c(cindex, graf)),
        names_to="measure", values_to="value") %>%
    select(clin, task_id, iteration, learner_id, measure, value) %>%
    left_join(bmr_imp %>%
        filter(learner_id=="clin_ref") %>%
        select(task_id, iteration, all_of(c(cindex, graf))) %>%
        pivot_longer(all_of(c(cindex, graf)), names_to="measure",
            values_to="value_ref")) %>%
    filter(measure=="graf_p0.9") %>%
    filter(learner_id=="rgcca_unsup_n1_tau0.1", clin=="without clinical data") %>%
    mutate(diff=value-value_ref) %>%
    ggplot(aes(x=diff)) +
    geom_histogram(bins=15, color="black", fill="white") +
    facet_wrap(.~task_id, scales="free") +
    geom_vline(xintercept=0, linetype="dashed")

bmr_imp %>%
    filter(type!="Reference") %>%
    pivot_longer(all_of(c(cindex, graf)),
        names_to="measure", values_to="value") %>%
    select(clin, task_id, iteration, learner_id, measure, value) %>%
    left_join(bmr_imp %>%
        filter(learner_id=="clin_ref") %>%
        select(task_id, iteration, all_of(c(cindex, graf))) %>%
        pivot_longer(all_of(c(cindex, graf)), names_to="measure",
            values_to="value_ref")) %>%
    filter(measure=="graf_p0.9") %>%
    filter(learner_id=="rgcca_sup_n1_tau1", clin=="without clinical data") %>%
    mutate(diff=value-value_ref) %>%
    ggplot(aes(x=diff, y=task_id)) +
    geom_boxplot()


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
    select(-fold) %>%
    mutate(clin=ifelse(grepl("clin", full_learner_id),
        "with clinical data", "without clinical data"))

bmr_cv = bmr_imp %>%
    left_join(cv_df, by=c("clin", "full_learner_id", "task_id", "iteration")) %>%
    filter(!is.na(cv_metric)) %>%
    group_by(type, clin, iteration, task_id) %>%
    slice_min(cv_metric)

bmr_cv %>%
    mutate(learner_id=type) %>%
    rbind(bmr_imp %>%
        filter(grepl("RGCCA", type)) %>%
        filter(clin=="with clinical data")
    ) %>%
    group_by(task_id, learner_id) %>%
    summarise(cindex_p0.9 = mean(cindex_p0.9))

yline = bmr_imp %>%
    filter(learner_id=="clin_ref") %>%
    group_by(task_id) %>%
    summarise(cindex_p0.9=mean(cindex_p0.9)) %>%
    pull(cindex_p0.9) %>% median
bmr_cv %>%
    mutate(learner_id=ifelse(type=="RGCCA Sup.", "cv_sup", "cv_unsup")) %>%
    rbind(bmr_imp %>%
        filter(!grepl("sup2", learner_id)) %>%
        filter(grepl("RGCCA", type)) %>%
        filter(clin=="with clinical data")
    ) %>%
    rbind(bmr_imp %>% filter(type=="Reference")) %>%
    group_by(task_id, learner_id, type) %>%
    summarise(cindex_p0.9 = mean(cindex_p0.9)) %>%
    ungroup() %>%
    mutate(type=factor(type), learner_id=factor(learner_id)) %>%
    mutate(learner_id=fct_reorder(learner_id, as.integer(type))) %>%
    arrange(type, learner_id) %>%
    mutate(cv=grepl("cv", learner_id)) %>%
    ggplot(aes(x=learner_id, y=cindex_p0.9, color=cv)) +
    geom_boxplot(aes(fill=type)) +
    geom_hline(yintercept=yline, linetype="dashed") +
    scale_colour_manual(values=c("black", "red"))

yline = bmr_imp %>%
    filter(learner_id=="clin_ref") %>%
    group_by(task_id) %>%
    summarise(graf_p0.9=mean(graf_p0.9)) %>%
    pull(graf_p0.9) %>% median
bmr_cv %>%
    mutate(learner_id=ifelse(type=="RGCCA Sup.", "cv_sup", "cv_unsup")) %>%
    rbind(bmr_imp %>%
        filter(!grepl("sup2", learner_id)) %>%
        filter(grepl("RGCCA", type)) %>%
        filter(clin=="with clinical data")
    ) %>%
    rbind(bmr_imp %>% filter(type=="Reference")) %>%
    group_by(task_id, learner_id, type) %>%
    summarise(graf_p0.9 = mean(graf_p0.9)) %>%
    ungroup() %>%
    mutate(type=factor(type), learner_id=factor(learner_id)) %>%
    mutate(learner_id=fct_reorder(learner_id, as.integer(type))) %>%
    arrange(type, learner_id) %>%
    mutate(cv=grepl("cv", learner_id)) %>%
    ggplot(aes(x=learner_id, y=graf_p0.9, colour=cv)) +
    geom_boxplot(aes(fill=type)) +
    geom_hline(yintercept=yline, linetype="dashed") +
    scale_colour_manual(values=c("black", "red"))

bmr_cv %>%
    pull(learner_id) %>%
    table()

bmr_cv %>%
    select(learner_id) %>%
    separate_wider_regex(learner_id,
        patterns=c("rgcca_", sup=".*",
                   "_n", n=".*",
                   "_tau", tau=".*"
    )) %>%
    group_by(sup, n, tau) %>%
    summarize(count=n()) %>%
    ggplot(aes(x=n, y=tau, fill=count)) +
    geom_tile() +
    geom_text(aes(label=count), colour="white") +
    facet_wrap(sup~.)

bmr_cv %>%
    select(learner_id) %>%
    separate_wider_regex(learner_id,
        patterns=c("rgcca_", sup=".*",
                   "_n", n=".*",
                   "_tau", tau=".*"
    )) %>%
    ggplot(aes(x=n, y=tau)) +
    geom_bin2d() +
    facet_wrap(sup~.)
