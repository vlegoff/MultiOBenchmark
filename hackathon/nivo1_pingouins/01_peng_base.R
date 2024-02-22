## Librairies
library(mlr3)
library(mlr3extralearners)
library(mlr3viz)

## Liste des taches prédéfinies
mlr_tasks

tsk_peng <- tsk("penguins")
tsk_peng # -> "multiclass" parce qu'il y a trois groupes
tsk_peng$data() # -> pour voir les data

## Automatic plot
autoplot(tsk_peng)

## Split train et test
splits <- partition(tsk_peng)

## Train. Pour chercher un learner : https://mlr-org.com/learners.html
lrn_rpart <- lrn("classif.rpart")
lrn_rpart$train(tsk_peng, splits$train)

## Test + score : https://mlr-org.com/measures.html
prediction <- lrn_rpart$predict(tsk_peng, splits$test)
prediction$score(msr("classif.acc"))
