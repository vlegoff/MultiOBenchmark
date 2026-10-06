library(multiMiR)
library(miRBaseConverter)
library(biomaRt)
library(parallel)
library(tidyverse)

ncpus = as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))

# first we get all mirnas from all datasets
cancers = c("LAML", "ESCA", "PAAD", "SARC", "LIHC", "COAD", "KIRP", "OV", "KIRC",
    "SKCM", "STAD", "BLCA", "UCEC", "LGG", "HNSC", "LUSC", "LUAD", "BRCA")
mirs = c()
for (nom_cancer in cancers) {
    print(nom_cancer)
    con = file(paste0("../benchmark/data/", nom_cancer, ".csv"))
    header = strsplit(readLines(con, n=1), ",")[[1]]
    close(con)
    new_mirs = gsub("_mirna$", "",
        grep("_mirna$", header, value=TRUE))
    mirs = c(mirs, setdiff(new_mirs, mirs))
}
mirs = gsub("\\.", "-", mirs)

mir_df = miRNA_PrecursorToMature(miRNANames=mirs, version = "v21")
setdiff(mirs, mir_df$OriginalName) #0 lost

mir_df = mir_df |>
    tidyr::pivot_longer(c(Mature1, Mature2),
        names_to="name", values_to="mature") |>
    subset(!is.na(mature), select=-c(name))

targets = mclapply(unique(mir_df$mature), \(m) {
    tryCatch(
        { suppressMessages(get_multimir(mirna=m, summary=FALSE)@data) },
        error = \(err) NULL
    )
}, mc.cores=ncpus)

targets_df = Reduce(
    rbind,
    lapply(targets[!sapply(targets, is.null)], \(t) {
        subset(t, select=c(mature_mirna_id, target_ensembl, target_symbol))}
    )
)

lost_mirs = setdiff(mir_df$mature, unique(targets_df$mature_mirna_id))
max_try = 10
n_try = 0
while(length(lost_mirs)>0 & n_try<=max_try) {
    message(n_try)
    more_targets = get_multimir(mirna=lost_mirs, summary=FALSE)@data
    targets_df = targets_df |>
        rbind(more_targets |>
            subset(select=c(mature_mirna_id, target_ensembl, target_symbol)))
    lost_mirs = setdiff(mir_df$mature, unique(targets_df$mature_mirna_id))
}

more_targets = get_multimir(mirna=lost_mirs, summary=FALSE)@data

targets_df = targets_df |>
    rbind(more_targets |>
        subset(select=c(mature_mirna_id, target_ensembl, target_symbol)))

# have to manually change hsa mir 1258 to match
targets_df = targets_df |>
    subset(!duplicated(targets_df)) |>
    transform(
        mature_mirna_id=ifelse(mature_mirna_id=="hsa-mir-1258",
            "hsa-miR-1258", mature_mirna_id))

mirna_map = mir_df |>
    rename(mature_mirna_id=mature) |>
    left_join(targets_df, relationship="many-to-many")

mirna_map |>
    subset(is.na(target_ensembl))

mirna_map |>
    subset(mature_mirna_id=="hsa-miR-1258")

test = get_multimir(mirna="hsa-miR-1258")

write.csv(mirna_map, "vlegoff_mirna_gene_map.csv")
