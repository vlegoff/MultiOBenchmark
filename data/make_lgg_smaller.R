library(dplyr)

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

## Création du "plus petit" jeu de données
lgg <- cbind(
  dat %>% select(bcr_patient_barcode, time, status, ends_with("_clinical")),
  mutation,
  cnv,
  rna,
  mirna
)

## Sauvegarde
save(lgg, file = "hackathon/minilgg.RData")
