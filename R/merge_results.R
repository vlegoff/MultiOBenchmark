suppressMessages({
    library(data.table)
    library(tidyverse)
    library(dtplyr)
    library(argparse)
})

parser = ArgumentParser()
parser$add_argument("-f", "--folder", nargs=1,
    help="Folder storing results")
parser$add_argument("-N", "--Ncpus", default=1,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-m", "--measures", nargs=1,
    help="")
parser$add_argument("-d", "--directory", default="results",
    help="where to store results")
parser$add_argument("-v", "--verbose", action="store_true",
    help="")
args = parser$parse_args()

data.table::setDTthreads(args$Ncpus)

data_dir = args$folder
results_dir = args$directory

registries = c(
    "ref",
    "AL",
    "AL_woClin",
    "RGCCA",
    "RGCCA_woClin",
    "SGCCA",
    "SGCCA_woClin",
    "PCA",
    "PCA_woClin",
    "JIVE",
    "JIVE_woClin",
    "SJIVE",
    "SJIVE_woClin",
    "intNMF",
    "intNMF_woClin",
    "MCIA",
    "MCIA_woClin",
    "MOFA",
    "MOFA_woClin",
    "iCluster",
    "iCluster_batch2",
    "iCluster_batch3",
    "iCluster_batch4",
    "tPCA",
    "tPCA_woClin",
    "tFOBI",
    "tFOBI_woClin"
)

measures = source(args$measures)
measures = sapply(measures$value, `[[`, "id")
cindex = measures[grepl("cindex", measures)]
graf = measures[grepl("graf", measures)]

replace_na = function(x, val, thresh=0.2) {
    na_rate = sum(is.na(x))/length(x)
    if (na_rate<thresh) {
        x[is.na(x)] = median(x, na.rm=TRUE)
    } else {
        x[is.na(x)] = val
    }
    return(x)
}

res_files = paste0(data_dir, "/", registries, "/", registries, "_results.csv")
cv_files = paste0(data_dir, "/cv_logs/", registries, "/raw_cv_results.csv")
coef_files = paste0(data_dir, "/cv_logs/", registries, "/raw_coef_results.csv")

res_files = res_files[file.exists(res_files)]
cv_files = cv_files[file.exists(cv_files)]
coef_files = coef_files[file.exists(coef_files)]

bmr = rbindlist(lapply(res_files, fread), fill=TRUE)
cv_df = rbindlist(lapply(cv_files, fread), fill=TRUE)
coef_df = rbindlist(lapply(coef_files, fread), fill=TRUE)

cols_to_keep = c("uhash", "nr", "task_id", "learner_id", "resampling_id",
    "iteration", "error", measures)
bmr = bmr[, ..cols_to_keep]

bmr_raw = bmr
bmr = bmr %>%
    mutate(clin=case_when(
        grepl("ref", learner_id) ~ "Reference",
        grepl("clin", learner_id) ~ "With clinical data",
        .default="Without clinical data"
    )) %>%
    mutate(method_name = case_when(
        grepl("rgcca_unsup", learner_id) ~ "RGCCA Unsup.",
        grepl("rgcca_sup", learner_id) ~ "RGCCA Sup.",
        grepl("sgcca_unsup", learner_id) ~ "SGCCA Unsup.",
        grepl("sgcca_sup", learner_id) ~ "SGCCA Sup.",
        grepl("sjive", learner_id) ~ "sJIVE", # always before JIVE
        grepl("jive", learner_id) ~ "JIVE",
        grepl("ipca", learner_id) ~ "PCA Indiv.",
        grepl("jpca", learner_id) ~ "PCA Joint",
        grepl("intnmf", learner_id) ~ "IntNMF",
        grepl("MCIA_Ntest", learner_id) ~ "MCIA Ntest",
        grepl("MCIA_Ntrain", learner_id) ~ "MCIA Ntrain",
        grepl("tfobi", learner_id) ~ "tPCA+tFOBI",
        grepl("tpca", learner_id) ~ "tPCA",
        grepl("mofa", learner_id) ~ "MOFA+MOTL",
        grepl("icluster", learner_id) ~ "iCluster",
        learner_id == "cox_ref" ~ "Cox clin.",
        learner_id == "glmnet_ref" ~ "Cox L2 clin.",
        learner_id == "kaplan_ref" ~ "Kaplan-Meier",
        grepl("blockforest", learner_id) ~ "BlockForest",
        learner_id == "prioritylasso_clin_fav" ~ "Priority-LASSO clin. fav.",
        grepl("prioritylasso", learner_id) ~ "Priority-LASSO",
        grepl("ipflasso", learner_id) ~ "IPF-LASSO",
        grepl("lasso", learner_id) ~ "LASSO",
        grepl("ranger", learner_id) ~ "ranger",
        .default=learner_id
    )) %>%
    #mutate(method_type = case_when(
    #    grepl("gcca", learner_id) ~ "RGCCA",
    #    grepl("jive", learner_id) ~ "JIVE",
    #    grepl("pca", learner_id) ~ "PCA",
    #    grepl("intnmf", learner_id) ~ "NMF",
    #    grepl("MCIA", learner_id) ~ "MCIA",
    #    grepl("tFOBI", learner_id) ~ "Tensor",
    #    grepl("mofa", learner_id) ~ "MOFA",
    #    grepl("icluster", learner_id) ~ "iCluster",
    #    clin == "Reference" ~ "Reference",
    #    .default="Previous study"
    #)) 
    mutate(method_type = case_when(
        grepl("gcca", learner_id) ~ "JDR - Covariance Based",
        grepl("jive", learner_id) ~ "JDR - Matrix Factorization",
        grepl("tFOBI", learner_id) ~ "JDR - Tensor Factorization",
        grepl("tpca", learner_id) ~ "JDR - Tensor Factorization",
        grepl("tfobi", learner_id) ~ "JDR - Tensor Factorization",
        grepl("pca", learner_id) ~ "PCA",
        grepl("intnmf", learner_id) ~ "JDR - Matrix Factorization",
        grepl("MCIA", learner_id) ~ "JDR - Covariance Based",
        grepl("mofa", learner_id) ~ "JDR - Matrix Factorization",
        grepl("icluster", learner_id) ~ "JDR - Matrix Factorization",
        method_name=="LASSO" ~ "Previous study - Naive",
        method_name=="ranger" ~ "Previous study - Naive",
        clin == "Reference" ~ "Reference",
        .default="Previous study - Structured"
    ))

bmr_comp = bmr %>%
    complete(task_id, learner_id, iteration) %>%
    left_join(cv_df)

bmr_imp = bmr_comp %>%
    group_by(task_id, learner_id) %>%
    #mutate(across(all_of(c(cindex, graf)), \(x) ifelse(x>1 | x<0, NA, x))) %>%
    mutate(across(all_of(c(cindex, graf)), \(x) ifelse(x<0, NA, x))) %>%
    mutate(across(all_of(cindex), \(x) replace_na(x, .5))) %>%
    mutate(across(all_of(graf), \(x) replace_na(x, .25))) %>%
    ungroup()

fwrite(bmr_raw, paste0(results_dir, "/results_aggregated_raw.csv"))
fwrite(bmr, paste0(results_dir, "/results_aggregated.csv"))
fwrite(bmr_comp, paste0(results_dir, "/results_aggregated_completed.csv"))
fwrite(bmr_imp, paste0(results_dir, "/results_aggregated_imputed.csv"))

# doing cross validation
bmr_cv = bmr_imp %>%
    select(-error) %>%
    mutate(cv_id = case_when(
        grepl("rgcca_unsup", learner_id) ~ "rgcca_unsup",
        grepl("rgcca_sup", learner_id) ~ "rgcca_sup",
        grepl("sgcca_unsup", learner_id) ~ "sgcca_unsup",
        grepl("sgcca_sup", learner_id) ~ "sgcca_sup",
        grepl("sjive", learner_id) ~ "sjive",
        grepl("jive", learner_id) ~ "jive",
        grepl("ipca", learner_id) ~ "ipca",
        grepl("jpca", learner_id) ~ "jpca",
        grepl("intnmf", learner_id) ~ "intnmf",
        grepl("MCIA_Ntest", learner_id) ~ "MCIA_Ntest",
        grepl("MCIA_Ntrain", learner_id) ~ "MCIA_Ntrain",
        grepl("tfobi", learner_id) ~ "tFOBI",
        grepl("tpca", learner_id) ~ "tPCA",
        grepl("mofa", learner_id) ~ "MOFA",
        grepl("icluster", learner_id) ~ "iCluster",
        .default=learner_id
    )) %>%
    mutate(cv_id = ifelse(
        grepl("clin_fav", learner_id) & !grepl("prioritylasso", learner_id),
        paste0(cv_id, "_clin_fav"), cv_id)) %>%
    #group_by(clin, task_id, learner_name, iteration) %>%
    slice_min(cv_metric, by=c(task_id, cv_id, iteration))

fwrite(bmr_cv, paste0(results_dir, "/results_cross_validated.csv"))
    
fwrite(bmr_cv, paste0(results_dir, "/results_cv_clean.csv"))

coef_df = coef_df %>%
    group_by(task_id, learner_id, iteration) %>%
    mutate(beta_norm = beta/norm(beta, "2")) %>%
    mutate(beta_norm2=beta_norm^2) %>%
    ungroup() %>%
    mutate(var_type=ifelse(grepl("clinical", comp), "clinical", "omics"))

fwrite(coef_df, paste0(results_dir, "/coef.csv"))
