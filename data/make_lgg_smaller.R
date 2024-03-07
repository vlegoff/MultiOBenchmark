## Librairies
library(biomaRt)
library(dplyr)
library(tidyr)

## Chargement des données
load("data/LGG.RData")

## Structure des blocs
table(gsub(".*_([a-z]+)$", "\\1", names(dat)))

## Quels sont les blocs
mutation <- dat[, grep("_mutation$", names(dat))]
cnv <- dat[, grep("_cnv$", names(dat))]
rna <- dat[, grep("_rna$", names(dat))]
mirna <- dat[, grep("_mirna$", names(dat))]

## Filtrage des données
mutation <- mutation[, colSums(mutation) > 1]
cnv <- cnv[, abs(colMeans(cnv)) >= 0.1]
rnavars <- apply(rna, 2, var)
rna <- rna[, order(-rnavars)][, 1:5000]

## Annotation des CNVs et RNAs
mart <- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))

ensembl_rna <- gsub("_[1-z]+$", "", names(rna))
ensembl_cnv <- gsub("_[1-z]+$", "", names(cnv))

annot_rna <- getBM(
  filters = "ensembl_gene_id",
  attributes = c("ensembl_gene_id", "hgnc_symbol"),
  values = ensembl_rna,
  mart = mart)
annot_cnv <- getBM(
  filters = "ensembl_gene_id",
  attributes = c("ensembl_gene_id", "hgnc_symbol"),
  values = ensembl_cnv,
  mart = mart)
annot_both <- rbind(annot_rna, annot_cnv) %>% 
  drop_na() %>%
  distinct() %>%
  mutate(
    rna_col = paste0(ensembl_gene_id, "_rna"),
    cnv_col = paste0(ensembl_gene_id, "_cnv"),
  )

selected_variables_rna <- intersect(annot_both$rna_col, names(rna))
selected_variables_cnv <- intersect(annot_both$cnv_col, names(cnv))

rna <- rna[, selected_variables_rna]
cnv <- cnv[, selected_variables_cnv]

names(rna) <- with(
  annot_both,
  paste0(make.unique(hgnc_symbol[
    match(selected_variables_rna, rna_col)]), "_rna"))
names(cnv) <- with(
  annot_both,
  paste0(make.unique(hgnc_symbol[
    match(selected_variables_cnv, cnv_col)]), "_cnv"))


## Création du "plus petit" jeu de données
lgg <- cbind(
  dat %>% dplyr::select(bcr_patient_barcode, time, status, ends_with("_clinical")),
  mutation,
  cnv,
  rna,
  mirna
)

names(lgg) <- gsub("-", "_", names(lgg))
lgg <- lgg %>% dplyr::select(-starts_with("_"), -starts_with("."))

## Sauvegarde
save(lgg, file = "hackathon/minilgg.RData")
