#' @title Survival Rgcca For Dimension Reduction Followed By Glmnet For Surv Prediction Learner
#' @author Unknown
#' @name mlr_learners_surv.rgcca
#'
#' @description
#' FIXME: BRIEF DESCRIPTION OF THE LEARNER.
#' Calls [RGCCA::RGCCA()] from FIXME: (CRAN VS NO CRAN): \CRANpkg{RGCCA} | 'RGCCA'.
#'
#' @section Initial parameter values:
#' FIXME: DEVIATIONS FROM UPSTREAM PARAMETERS. DELETE IF NOT APPLICABLE.
#'
#' @section Custom mlr3 parameters:
#' FIXME: DEVIATIONS FROM UPSTREAM DEFAULTS. DELETE IF NOT APPLICABLE.
#'
#' @templateVar id surv.rgcca
#' @template learner
#'
#' @references
#' `r format_bib(FIXME: ONE OR MORE REFERENCES FROM bibentries.R)`
#'
#' @template seealso_learner
#' @template example
#' @export

suppressPackageStartupMessages({library(R6)})

LearnerSeqMod = R6Class("LearnerSeqMod",
  inherit = mlr3proba::LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set=ps(), predict_types="distr",
                          feature_types=character(), properties=character(),
                          packages=character(),
                          label=NA_character_, man=NA_character_) {

      necessary_pars = c("blocks", "clinical_fav", "nlambdas", "nfolds",
        "CV_measure", "seed")
        # check if necessary pars are in pars set
        if (!all(necessary_pars %in% param_set$ids())) {
            stop("missing parameters")
        }

        packages = c(packages, "glmnet", "survival", "survAUC", "SurvMetrics",
            "caret", "data.table", "mlr3extralearners")

      super$initialize(
        id = id,
        param_set=param_set, 
        predict_types = predict_types,
        feature_types = feature_types,
        properties = properties,
        packages = packages,
        label = label,
        man = man
      )
    }
  ),
  private = list(
    train_jdr = function(x, y, pars) {
        warning("not implemented, abstract class")
        return(list(x=as.matrix(x), jdr=NULL))
    },
    predict_jdr = function(newx, jdr, pars) {
        warning("not implemented, abstract class")
        return(newx)
    },
    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags="train")

      # Load data
      x = task$data(cols=task$feature_names)
      ysurv = task$truth()

      # setting seed for internal CV reproducibility
      set.seed(pars$seed)

      # Some checks

      # clinical must not be present in the blocks
      if (("clinical" %in% pars$blocks) & pars$clinical_fav)
        stop("Clinical cannot be in both blocks and latent space")

      # ncomp must be inferior or equal to smallest block
      # add check

      # Get blocks
      block_list = lapply(
        pars$blocks,
        function(bl) {
            as.matrix(x[, .SD, .SDcols=names(x) %like% paste0("_", bl, "$")])
        }
      )
      names(block_list) <- pars$blocks

      JDR = private$train_jdr(block_list, ysurv, pars)

      if (pars$clinical_fav) { # adding clinical data if necessary
        clinicals = as.matrix(x[, .SD, .SDcols=names(x) %like% "_clinical$"])
        JDR$x = cbind(JDR$x, clinicals)
      }

      # implement case where there is only 1 comp and no clinical data
      # it corresponds to a simple coxph model, no need for CV
      if (ncol(JDR$x)==1) {
        coxph = survival::coxph(ysurv ~ ., data=as.data.frame(JDR$x))
        return(list(JDR=JDR$jdr, coxph=coxph))  
      }

      #Fit glmnet to get lambda sequence used in internal CV loop
      glmnet_fit = glmnet::glmnet(JDR$x, ysurv, family="cox", alpha=0,
                                  nlambda=pars$nlambdas)
      lambdas = glmnet_fit$lambda

      folds = caret::createFolds(ysurv[, "status"], k=pars$nfolds, list=TRUE)

      #results = vector("list", length=pars$nfolds)
      results = matrix(NA, nrow=pars$nfolds, ncol=pars$nlambdas)
      for (i in seq_along(folds)) { # Internal CV loop
        #train
        f = folds[[i]]
        ytrain = ysurv[-f]
        fold_train = lapply(block_list, function(x) x[-f,])

        JDR_train = private$train_jdr(fold_train, ytrain, pars)

        if(pars$clinical_fav) {
          JDR_train$x = cbind(JDR_train$x, clinicals[-f,])
        }
        
        # fit glmnet on train data
        glmnet_train = glmnet::glmnet(JDR_train$x, ytrain,
                                      family="cox", alpha=0, lambda=lambdas)

        #test
        ytest = ysurv[f]
        fold_test = lapply(
          block_list, function(x) x[f,]
        )
        names(fold_test) = names(block_list)

        JDR_test = private$predict_jdr(fold_test, JDR_train$jdr, pars)
        if(pars$clinical_fav) {
          JDR_test$x = cbind(JDR_test$x, clinicals[f,])
        }

        # Computing CV measure
        if (pars$CV_measure == "cindex") {
          fit = predict(glmnet_train, newx=JDR_test$x)
          res = apply(fit, 2, function(lp) survAUC::UnoC(ytrain, ytest, lp))
        } else if (pars$CV_measure == "ibrier") {
          fit = survival::survfit(glmnet_train, x=JDR_train$x,
                                  y=ytrain, weights=glmnet_train$weights,
                                  newx=JDR_test$x, se.fit=FALSE)
          probs = lapply(fit, getElement, name="surv")
          res = sapply(
              probs,
              function(p) 
                  SurvMetrics::IBS(ytest, t(p),
                                   IBSrange=c(min(ysurv[,"time"]),
                                              max(ysurv[,"time"])))
          )
        } else if (pars$CV_measure == "auc") {
          fit = predict(glmnet_train, newx=JDR_test$x)
          res = apply(fit, 2, function(lp) survAUC::AUC.uno(ytrain, ytest, lp))
        }
        #results = c(results, list(res))
        results[i, ] = res

      }
      #results = Reduce(rbind, results)

      select_best = ifelse(pars$CV_measure %in% c("cindex, auc"),
                           which.max,
                           which.min)
      mean_results = apply(results, 2, mean, na.rm=TRUE)
      best_lambda = lambdas[select_best(mean_results)]

      list(JDR=JDR$jdr,
           glmnet=glmnet_fit,
           best_lambda=best_lambda,
           cv_grid=results,
           folds=folds,
           y=ysurv,
           x=JDR$x)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags="predict")

      # get newdata and ensure same ordering in train and predict
      newx = mlr3extralearners:::ordered_features(task, self)
      block_list = lapply(
        pars$blocks,
        function(bl) as.matrix(newx[,
          .SD,
          .SDcols=names(newx) %like% paste0("_", bl, "$")
        ])
      )
      names(block_list) = pars$blocks

      JDR_new = private$predict_jdr(block_list, self$model$JDR, pars)

      if(pars$clinical_fav) {
        clinicals = as.matrix(newx[, .SD, .SDcols=names(newx) %like% "clinical"])
        JDR_new$x = cbind(JDR_new$x, clinicals)
      }

      # if there is only one comp
      if (ncol(JDR_new$x)==1) {
        fit = survival::survfit(self$model$coxph,
                                newdata=as.data.frame(JDR_new$x),
                                se.fit=FALSE)
        lp = predict(self$model$coxph,
                       newdata=as.data.frame(JDR_new$x))
        return(mlr3proba::.surv_return(times=fit$time,
                                       surv=t(fit$surv),
                                       lp=lp))
      }
      
      # Calculate predictions for the selected predict type.
      which_lambda = which(self$model$best_lambda==self$model$glmnet$lambda)
      fit = survival::survfit(self$model$glmnet,
                              s=self$model$best_lambda,
                              x=self$model$x,
                              y=self$model$y,
                              newx=JDR_new$x,
                              se.fit=FALSE)
      lp = predict(self$model$glmnet,
                   newx=JDR_new$x,
                   s=self$model$best_lambda)

      return(mlr3proba::.surv_return(times=fit$time,
                                     surv=t(fit$surv),
                                     lp=lp))

    }
  )
)
