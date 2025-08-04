library(data.table)
library(tidyverse)

clin_registry = c("ref",
    "AL",
    "RGCCA",
    #"RGCCA_test_clin",
    "SGCCA",
    "JIVE",
    "PCA",
    "SJIVE",
    #"SJIVE2",
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
    #"SJIVE2_woClin",
    "intNMF_woClin",
    "MOFA_woClin",
    "MOFA_hvg_woClin")
woclin_registry = paste0(woclin_registry, "/", woclin_registry, "_results.csv")
tbls = lapply(woclin_registry, fread)
table_woclin = rbindlist(tbls, fill=TRUE)
table_woclin[,clin:="without clinical data"]

bmr = rbind(table_clin, table_woclin) %>% 
    mutate(clin=ifelse(grepl("ref", learner_id), "reference", clin))
fwrite(bmr, "results_aggregated_raw.csv")

bmr %<>%
    mutate(full_learner_id=learner_id) %>%
    mutate(learner_id = ifelse(!grepl("prioritylasso", learner_id),
        str_replace(learner_id, "_clin_fav", ""),
        learner_id)) %>%
        complete(task_id, iteration, nesting(learner_id, clin)) %>%
        filter(!(learner_id %in% c("clin_ref", "glmnet_clin", "kaplan",
                                   "prioritylasso_clin_fav") 
                 & clin=="without clinical data")) %>%
        mutate(type=case_when(
            clin=="reference" ~ "Reference",
            learner_id %in% c("ranger", "blockforest") ~ "Prev. study",
            #learner_id %in% c("ranger_clin", "blockforest_clin") ~ "Prev. study",
            grepl("lasso", learner_id) ~ "Prev. study",
            grepl("rgcca_sup2", learner_id) ~ "RGCCA Sup. Clin.",
            grepl("rgcca_sup", learner_id) ~ "RGCCA Sup.",
            grepl("rgcca_unsup", learner_id) ~ "RGCCA Unsup.",
            grepl("sgcca_sup", learner_id) ~ "SGCCA Sup.",
            grepl("sgcca_unsup", learner_id) ~ "SGCCA Unsup.",
            grepl("sjive", learner_id) ~ "sJIVE",
            #grepl("[ja]0", learner_id) ~ "PCA",
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
        filter(learner_id=="cox_ref") %>%
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
    group_by(clin, type, task_id, learner_id) %>%
    summarise(across(all_of(measures), mean)) %>%
    select(clin, type, task_id, learner_id, cindex_p0.9, graf_p0.9) %>%
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

library(ggpubr)
bmr_for_test %>%
    filter(task_id=="UCEC", learner_id=="rgcca_sup_n1_tau1",
           measure=="graf") %>%
    pivot_longer(c(value, value_ref), names_to="learner",
        values_to="Graf Score") %>%
    mutate(learner=ifelse(learner=="value_ref", "Cox model", "RGCCA")) %>%
    ggpaired(x="learner", y="Graf Score", color="learner", id="iteration",
             line.size=0.4, palette="jco", line.color="gray") +
    stat_compare_means(paired=TRUE, method="t.test")

bmr_for_test %>%
    filter(learner_id=="rgcca_sup_n1_tau1",
           measure=="graf") %>%
    mutate(id=paste(task_id, iteration)) %>%
    pivot_longer(c(value, value_ref), names_to="learner",
        values_to="Graf Score") %>%
    mutate(learner=ifelse(learner=="value_ref", "Cox model", "RGCCA")) %>%
    ggpaired(x="learner", y="Graf Score", color="learner", id="id",
             line.size=0.4, palette="jco", line.color="gray") +
    stat_compare_means(paired=TRUE, method="t.test")

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
        filter(learner_id=="cox_ref") %>%
        select(task_id, iteration, all_of(c(cindex, graf))) %>%
        pivot_longer(all_of(c(cindex, graf)), names_to="measure",
            values_to="value_ref")) %>%
    filter(measure=="graf_p0.9") %>%
    filter(learner_id=="rgcca_unsup_n1_tau0.1", clin=="with clinical data") %>%
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
        filter(learner_id=="cox_ref") %>%
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
# - better way to check clin/non clin learners

folds = readRDS("../data/folds.rds")
#cv_files = list.files("cv_logs", recursive=TRUE, full.names=TRUE,
    #pattern="*.rds")
cv_files = c(
    list.files("cv_logs/SJIVE", recursive=TRUE, full.names=TRUE,
        pattern="*.rds"),
    list.files("cv_logs/SJIVE_woClin", recursive=TRUE, full.names=TRUE,
        pattern="*.rds")
)
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

new_coef_dt = rbindlist(lapply(cv_results, \(x) data.table(
    full_learner_id = x$learner_id,
    task_id = x$task_id,
    fold=paste(x$fold, collapse=" "),
    beta=x$stanbeta,
    comp=names(x$stanbeta)))) %>%
    left_join(folds_df, by=c("task_id", "fold")) %>%
    mutate(clin=ifelse(grepl("clin", full_learner_id),
        "with clinical data", "without clinical data"))

coef_dt = fread("coef.csv")

coef_dt = coef_dt %>%
    select(full_learner_id, task_id, iteration, clin) %>%
    setdiff(new_coef_dt %>% select(full_learner_id, task_id, iteration, clin)) %>%
    left_join(coef_dt) %>%
    rbind(new_coef_dt) 

fwrite(coef_dt, "coef.csv")

coef_dt %>%
    filter(clin=="without clinical data") %>%
    filter(task_id!="LAML") %>%
    group_by(task_id, full_learner_id) %>%
    summarise(beta=sqrt(sum(beta^2))) %>%
    ggplot(aes(x=task_id, y=beta)) +
    geom_boxplot() +
    scale_y_log10()

coef_dt %>%
    group_by(full_learner_id) %>%
    mutate(beta = beta^2) %>%
    mutate(beta=beta/sum(beta)) %>%
    ungroup() %>%
    filter(clin=="with clinical data") %>%
    mutate(clinvar=grepl("clin", comp)) %>%
    left_join(bmr_imp, by=c("clin", "full_learner_id", "task_id", "iteration")) %>%
    select(task_id, iteration, full_learner_id, learner_id, clin, type, beta,
           clinvar) %>%
    group_by(learner_id, clinvar, type) %>%
    summarize(beta=sum(beta)) %>%
    mutate(clinvar=ifelse(clinvar, "clinical", "omics")) %>%
    filter(clinvar=="omics") %>%
    ggplot(aes(x=type, y=beta, fill=type)) +
    geom_boxplot()

coef_dt %>%
    filter(clin=="with clinical data") %>%
    group_by(full_learner_id, task_id) %>%
    mutate(beta = beta^2) %>%
    mutate(beta=beta/sum(beta)) %>%
    ungroup() %>%
    mutate(clinvar=grepl("clin", comp)) %>%
    left_join(bmr_imp, by=c("clin", "full_learner_id", "task_id", "iteration")) %>%
    select(task_id, iteration, full_learner_id, learner_id, clin, type, beta,
           clinvar) %>%
    group_by(full_learner_id, clinvar, type, task_id) %>%
    summarize(beta=sum(beta)) %>%
    mutate(clinvar=ifelse(clinvar, "clinical", "omics")) %>%
    filter(clinvar=="omics") %>%
    ggplot(aes(x=type, y=beta, fill=type)) +
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(alpha=.3)

new_cv = tibble(
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

cv_df = read.csv("raw_cv_results.csv")

cv_df = cv_df %>%
    select(full_learner_id, task_id, iteration, clin) %>%
    setdiff(new_cv %>% select(full_learner_id, task_id, iteration, clin)) %>%
    left_join(cv_df) %>%
    rbind(new_cv) 

cv_df %>%
    select(full_learner_id, task_id, iteration, clin) %>%
    setdiff(new_cv %>% select(full_learner_id, task_id, iteration, clin)) %>%
    pull(full_learner_id) %>%
    unique()

fwrite(cv_df, "raw_cv_results.csv")

bmr_cv = bmr_imp %>%
    left_join(cv_df, by=c("clin", "full_learner_id", "task_id", "iteration")) %>%
    filter(!is.na(cv_metric)) %>%
    mutate(type=ifelse(type=="PCA", "JIVE", type)) %>%
    group_by(type, clin, iteration, task_id) %>%
    slice_min(cv_metric)

bmr_cv %>%
    mutate(learner_id=type) %>%
    rbind(bmr_imp %>%
        filter(grepl("RGCCA", type)) %>%
        filter(clin=="with clinical data")
    ) %>%
    group_by(task_id, learner_id) %>%
    summarise(cindex_p0.9 = mean(cindex_p0.9),
        graf_p0.9=mean(graf_p0.9))

yline = bmr_imp %>%
    filter(learner_id=="cox_ref") %>%
    group_by(task_id) %>%
    summarise(cindex_p0.9=mean(cindex_p0.9),
              graf_p0.9=mean(graf_p0.9)) %>%
    ungroup() %>%
    pivot_longer(c(graf_p0.9, cindex_p0.9), names_to="measure",
                 values_to="value") %>%
    mutate(measure=str_replace(measure, "_p0.9", "")) %>%
    group_by(measure) %>%
    summarise(yline=median(value))
bmr_cv %>%
    #mutate(learner_id=ifelse(grepl("GCCA Sup.", type), "cv_sup", "cv_unsup")) %>%
    mutate(learner_id=type, cv=TRUE) %>%
    rbind(bmr_imp %>%
        filter(!grepl("sup2", learner_id)) %>%
        filter(!grepl("sJIVE", type)) %>%
        filter(grepl("GCCA", type) | grepl("JIVE", type)) %>%
        mutate(cv=FALSE)
    ) %>%
    rbind(bmr_imp %>% filter(type=="Reference")) %>%
    pivot_longer(c(graf_p0.9, cindex_p0.9), names_to="measure",
                 values_to="value") %>%
    mutate(measure=str_replace(measure, "_p0.9", "")) %>%
    group_by(clin, task_id, learner_id, type, measure, cv) %>%
    summarise(value = mean(value)) %>%
    ungroup() %>%
    mutate(type=factor(type), learner_id=factor(learner_id)) %>%
    mutate(learner_id=fct_reorder(learner_id, as.integer(type))) %>%
    arrange(type, learner_id) %>%
    #mutate(cv=grepl("cv", learner_id)) %>%
    left_join(yline) %>%
    ggplot(aes(x=learner_id, y=value, color=cv)) +
    geom_boxplot(aes(fill=type)) +
    geom_hline(aes(yintercept=yline), linetype="dashed") +
    scale_colour_manual(values=c("black", "red")) +
    #facet_grid(measure~clin, scales="free", space="free") +
    facet_grid(measure~clin, scales="free", space="free_x") +
    scale_x_discrete(guide=guide_axis(angle=45))

library(ggthemes)
yline = bmr_imp %>%
    filter(learner_id=="cox_ref") %>%
    group_by(task_id) %>%
    summarise(cindex_p0.9=mean(cindex_p0.9),
              graf_p0.9=mean(graf_p0.9)) %>%
    ungroup() %>%
    pivot_longer(c(graf_p0.9, cindex_p0.9), names_to="measure",
                 values_to="value") %>%
    mutate(measure=str_replace(measure, "_p0.9", "")) %>%
    group_by(measure) %>%
    summarise(yline=median(value))
bmr_cv %>%
    mutate(learner_id=type) %>%
    rbind(bmr_imp %>% filter(type=="Reference")) %>%
    pivot_longer(c(graf_p0.9, cindex_p0.9), names_to="measure",
                 values_to="value") %>%
    mutate(measure=str_replace(measure, "_p0.9", "")) %>%
    group_by(clin, task_id, type, measure, learner_id) %>%
    summarise(value = mean(value)) %>%
    ungroup() %>%
    mutate(type=factor(type)) %>%
    mutate(learner_id=case_when(
        learner_id == "cox_ref" ~ "Cox clin.",
        learner_id == "glmnet_ref" ~ "Cox L2 clin.",
        learner_id == "kaplan_ref" ~ "Kaplan-Meier",
        .default=learner_id)) %>%
    mutate(learner_id=forcats::fct_relevel(learner_id,
        "Kaplan-Meier", "Cox clin.", "Cox L2 clin.", "IntNMF", "JIVE", "sJIVE",
        "RGCCA Unsup.", "SGCCA Unsup.", "RGCCA Sup.", "SGCCA Sup.")) %>%
    mutate(type=forcats::fct_relevel(type,
        "Reference", "IntNMF", "JIVE", "sJIVE",
        "RGCCA Unsup.", "SGCCA Unsup.", "RGCCA Sup.", "SGCCA Sup.")) %>%
    left_join(yline) %>%
    ggplot(aes(x=learner_id, y=value)) +
    geom_boxplot(aes(fill=type)) +
    geom_hline(aes(yintercept=yline), linetype="dashed") +
    facet_grid(measure~clin, scales="free", space="free_x") +
    theme_bw() #+
    #scale_fill_few()

bmr_cv %>%
    pull(learner_id) %>%
    table()

bmr_cv %>%
    filter(clin=="without clinical data") %>%
    filter(grepl("rgcca", learner_id)) %>%
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
    filter(clin=="without clinical data") %>%
    filter(grepl("sgcca", learner_id)) %>%
    select(learner_id) %>%
    separate_wider_regex(learner_id,
        patterns=c("sgcca_", sup=".*",
                   "_n", n=".*",
                   "_s", s=".*"
    )) %>%
    group_by(sup, n, s) %>%
    summarize(count=n()) %>%
    ggplot(aes(x=n, y=s, fill=count)) +
    geom_tile() +
    geom_text(aes(label=count), colour="white") +
    facet_wrap(sup~.)

bmr_cv %>%
    filter(clin=="without clinical data") %>%
    filter(!grepl("sjive", learner_id)) %>%
    filter(grepl("jive", learner_id)) %>%
    select(learner_id) %>%
    separate_wider_regex(learner_id,
        patterns=c("jive",
                   "_j", j=".*",
                   "_a", a=".*"
    ), too_few="debug") %>%
    group_by(j, a) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ggplot(aes(x=j, y=a, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop), colour="white")

bmr_cv %>%
    filter(clin=="with clinical data") %>%
    filter(grepl("intnmf", learner_id)) %>%
    select(learner_id) %>%
    separate_wider_regex(learner_id,
        patterns=c("intnmf",
                   "_k", k=".*"
    ), too_few="debug") %>%
    group_by(k) %>%
    summarize(count=n()) %>%
    ggplot(aes(x=k, y=k, fill=count)) +
    geom_tile() +
    geom_text(aes(label=count), colour="white")

bmr_cv %>%
    filter(clin=="without clinical data") %>%
    filter(grepl("sjive", learner_id)) %>%
    select(learner_id) %>%
    separate_wider_regex(learner_id,
        patterns=c("sjive",
                   "_j", j=".*",
                   "_a", a=".*",
                   "_eta", eta=".*"
    ), too_few="debug") %>%
    group_by(j, a, eta) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ggplot(aes(x=j, y=a, fill=count)) +
    geom_tile() +
    geom_text(aes(label=count), colour="white") +
    facet_wrap(.~eta)

coef_dt %>%
    left_join(cv_df %>%
        select(full_learner_id, task_id, iteration, clin, cv_metric)) %>%
    left_join(bmr_imp %>% select(full_learner_id, task_id, iteration, clin,
                                 type)) %>%
    filter(!is.na(cv_metric)) %>%
    mutate(type=ifelse(type=="PCA", "JIVE", type)) %>%
    group_by(type, clin, iteration, task_id) %>%
    slice_min(cv_metric) %>%
    filter(clin=="with clinical data") %>%
    mutate(clinvar=grepl("clin", comp)) %>%
    mutate(clinvar=ifelse(clinvar, "clinical", "omics")) %>%
    group_by(type, task_id, iteration) %>%
    mutate(beta = beta^2) %>%
    mutate(beta=beta/sum(beta)) %>%
    ungroup() %>%
    filter(clinvar=="omics") %>%
    group_by(type, task_id, iteration) %>%
    summarize(beta=sum(beta)) %>%
    group_by(type, task_id) %>%
    summarize(beta=mean(beta)) %>%
    ungroup() %>%
    mutate(type=forcats::fct_relevel(type,
        "IntNMF", "JIVE", "sJIVE",
        "RGCCA Unsup.", "SGCCA Unsup.", "RGCCA Sup.", "SGCCA Sup.")) %>%
    ggplot(aes(x=type, y=beta, fill=type)) +
    #geom_violin()
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(alpha=.3) +
    theme_bw()

coef_dt %>%
    left_join(cv_df %>%
        select(full_learner_id, task_id, iteration, clin, cv_metric)) %>%
    left_join(bmr_imp %>% select(full_learner_id, task_id, iteration, clin,
                                 type)) %>%
    filter(!is.na(cv_metric)) %>%
    mutate(type=ifelse(type=="PCA", "JIVE", type)) %>%
    group_by(type, clin, iteration, task_id) %>%
    slice_min(cv_metric) %>%
    filter(clin=="with clinical data") %>%
    mutate(clinvar=grepl("clin", comp)) %>%
    mutate(clinvar=ifelse(clinvar, "clinical", "omics")) %>%
    group_by(type, task_id, iteration) %>%
    mutate(beta = beta^2) %>%
    mutate(beta=beta/sum(beta)) %>%
    ungroup() %>%
    filter(clinvar=="omics") %>%
    group_by(type, task_id, iteration) %>%
    summarize(beta=sum(beta)) %>%
    ungroup() %>%
    mutate(type=forcats::fct_relevel(type,
        "IntNMF", "JIVE", "sJIVE",
        "RGCCA Unsup.", "SGCCA Unsup.", "RGCCA Sup.", "SGCCA Sup.")) %>%
    ggplot(aes(x=type, y=beta, fill=type)) +
    #geom_violin()
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(alpha=.3) +
    theme_bw() +
    facet_wrap(.~task_id)
