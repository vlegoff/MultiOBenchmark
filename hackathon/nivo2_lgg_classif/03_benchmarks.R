## Packages
library(mlr3verse)
library(ggplot2)

## Pour paramétrer le thème ggplot
theme_set(theme_bw())

## Data
load("hackathon/minilgg.RData")
y <- factor(lgg$histological_type_Oligoastrocytoma_clinical)
mut <- cbind(y = y, lgg[, grep("_mutation$", names(lgg))])
rna <- cbind(y = y, lgg[, grep("_rna$", names(lgg))])
cnv <- cbind(y = y, lgg[, grep("_cnv$", names(lgg))])

## Tache + learner
tsk_oneblock <- list(
  TaskClassif$new("mut", mut, target = "y", positive = "1"),
  TaskClassif$new("rna", rna, target = "y", positive = "1"),
  TaskClassif$new("cnv", cnv, target = "y", positive = "1"))
  
lrn_list <- c(
  as_learner(po("removeconstants") %>>% lrn("classif.lda")),
  lrn("classif.svm"))

cv <- rsmp("cv", folds = 5)
grid <- benchmark_grid(tsk_oneblock, lrn_list, cv)
bmr <- benchmark(grid)

autoplot(bmr)

