## Librairies
library(mlr3)
library(mlr3tuning)
library(mlr3extralearners)
library(mlr3viz)
library(paradox)

## Pour paramétrer le thème ggplot
theme_set(theme_bw())

## Tache + learner
tsk_peng <- tsk("penguins")
lrn_rpart <- lrn("classif.rpart", xval = to_tune(0, 1000))
split <- partition(tsk_peng)

## Comment déterminer la valeur optimale de xval?
tnr_grid_search <- tnr("grid_search", resolution = 20)
at <- auto_tuner(
  tuner = tnr_grid_search,
  learner = lrn_rpart,
  resampling = rsmp("cv", folds = 3)
) 
at$train(tsk_peng, row_ids = split$train)
at$predict(tsk_peng, row_ids = split$test)$score()
at$tuning_instance$result
View(as.data.table(at$archive))
