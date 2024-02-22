## Librairies
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(mlr3)
library(mlr3extralearners)
library(mlr3viz)

## Pour paramétrer le thème ggplot
theme_set(theme_bw())

## Tache + learner
tsk_peng <- tsk("penguins")
lrn_rpart <- lrn("classif.rpart")

cv_rep <- rsmp("repeated_cv", repeats = 10, folds = 5)
cv_rep$instantiate(tsk_peng) # Superflu
peng_resamp <- resample(tsk_peng, lrn_rpart, cv_rep)
autoplot(peng_resamp, measure = msr("classif.acc"))

## Plot "à la main"
peng_resamp$score() %>% 
  ggplot(aes(x = 0, y = classif.ce)) + 
  geom_violin(trim = FALSE, width = 0.15) +
  geom_beeswarm() + 
  labs(x = "", y = "Classification error")
