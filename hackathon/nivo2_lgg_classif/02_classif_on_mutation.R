## Packages
library(mlr3verse)
library(ggplot2)

## Pour paramétrer le thème ggplot
theme_set(theme_bw())

## Data
load("hackathon/minilgg.RData")
mutation <- lgg[, grep("_mutation$", names(lgg))]
y <- factor(lgg$histological_type_Oligoastrocytoma_clinical)
dat_4_classif <- cbind(y = y, mutation)

## Tache + learner
tsk_lgg <- TaskClassif$new("lgg", dat_4_classif, target = "y", positive = "1")
po_lda <- po("removeconstants") %>>% lrn("classif.lda")
lrn_lda <- as_learner(po_lda)

cv_rep <- rsmp("repeated_cv", repeats = 2, folds = 3)
lgg_resamp <- resample(tsk_lgg, lrn_lda, cv_rep)
autoplot(lgg_resamp, measure = msr("classif.acc"))


