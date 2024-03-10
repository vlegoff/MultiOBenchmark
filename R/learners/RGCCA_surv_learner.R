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

LearnerSurvrgcca = R6Class("LearnerSurvrgcca",
  inherit = mlr3proba::LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        blocks=p_uty(tags=c("train", "predict")),
        tau=p_dbl(0, 1, default=1, tags=c("train")),
        clinical_fav=p_lgl(default=TRUE, tags=c("train", "predict")),
        supervised=p_lgl(default=FALSE, tags=c("train", "predict")),
        ncomp=p_int(1L, default=1L, tags=c("train")),
        scheme=p_fct(c("horst", "factorial", "centroid"), default="factorial",
                      tags=c("train")),
        nfolds=p_int(1L, default=10L, tags=c("train")),
        CV_measure=p_fct(c("cindex", "ibrier", "auc"), default="cindex",
                          tags=c("train")),
        seed=p_int(0L, special_vals=list(NULL), default=NULL, tags=c("train"))
      )
      param_set$values$tau = 1
      param_set$values$clinical_fav = TRUE
      param_set$values$supervised = FALSE
      param_set$values$ncomp = 1L
      param_set$values$scheme = "factorial"
      param_set$values$nfolds = 10L
      param_set$values$CV_measure = "cindex"
      param_set$values$seed = NULL

      super$initialize(
        id = "surv.rgcca",
        packages = c("RGCCA", "glmnet", "survival", "survAUC", "SurvMetrics",
                     "caret", "data.table"),
        feature_types = c("integer", "numeric", "factor"),
        predict_types = c("crank", "lp", "distr"),
        param_set = param_set,
        properties = c(),
        man = "mlr3extralearners::mlr_learners_surv.rgcca",
        label = "Regularized Generalized Canonical Correlation Analysis for Survival"
      )
    }
  ),
  private = list(
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
        function(bl) x[, .SD, .SDcols=names(x) %like% paste0("_", bl, "$")]
      )
      names(block_list) <- pars$blocks

      # Getting residuals if needed and creating connection matrix
      if(pars$supervised) {
        null_mod = survival::coxph(ysurv~1)
        block_list[["residuals"]] = residuals(null_mod, type="deviance")
      }
      complete_matrix = matrix(1, length(pars$blocks), length(pars$blocks))
      #for (i in seq_along(blocks)) complete_matrix[i,i] = 0
      diag(complete_matrix) = 0
      rownames(complete_matrix) = pars$blocks
      colnames(complete_matrix) = pars$blocks

      # Define RGCCA args
      rgcca_args = list(
        response=if(pars$supervised) length(block_list),
        connection=if(!pars$supervised) complete_matrix,
        tau=c(rep(pars$tau, length(block_list)-1), pars$tau*(!pars$supervised)),
        ncomp=pars$ncomp,
        scheme=pars$scheme,
        method="rgcca",
        scale=TRUE,
        scale_block="inertia",
        verbose=F
      )

      # compute RGCCA on whole dataset
      fit_rgcca = do.call(RGCCA::rgcca,
                          c(list(blocks=lapply(block_list, as.matrix)),
                            rgcca_args))
      latent_space = Reduce(cbind,
                            fit_rgcca$Y[names(fit_rgcca$Y)!="residuals"])

      if (pars$clinical_fav) { # adding clinical data if necessary
        clinicals = x[, .SD, .SDcols=names(x) %like% "_clinical$"]
        latent_space = cbind(latent_space, as.matrix(clinicals))
      }

      #Fit glmnet to get lambda sequence used in internal CV loop
      glmnet_fit = glmnet::glmnet(latent_space, ysurv, family="cox", alpha=0,
                                  nlambda=100)
      lambdas = glmnet_fit$lambda

      folds = caret::createFolds(ysurv[, "status"], k=pars$nfolds, list=TRUE)

      results = list() # should be vector("list", length=10)
      for (f in folds) { # Internal CV loop

        #train
        ytrain = ysurv[-f]
        fold_train = lapply(block_list, function(x) as.matrix(x[-f]))
        if(pars$supervised) { # get residuals on train set
          null_mod = survival::coxph(ytrain~1)
          fold_train[["residuals"]] = residuals(null_mod, type="deviance")
        }

        # compute latent space of training data
        rgcca_train = do.call(RGCCA::rgcca,
                              c(list(blocks=fold_train), rgcca_args))
        latent_space_train = Reduce(
          cbind,
          rgcca_train$Y[names(rgcca_train$Y)!="residuals"]
        )
        if(pars$clinical_fav) {
          latent_space_train = cbind(latent_space_train,
                                     as.matrix(clinicals[-f]))
        }
        
        # fit glmnet on train data
        glmnet_train = glmnet::glmnet(latent_space_train, ytrain,
                                      family="cox", alpha=0, lambda=lambdas)

        #test
        ytest = ysurv[f]
        fold_test = lapply(
          names(block_list),
          function(n) {
            ret = as.matrix(block_list[[n]][f])
            #colnames(ret) = paste0(n, "_", colnames(ret)) # useless now
            ret
          }
        )
        names(fold_test) = names(block_list)
        if(pars$supervised) {
          # actual residuals are not needed for prediction
          # any random values will work
          #null_mod = survival::coxph(ytest~1)
          fold_test[["residuals"]] = as.matrix(rep(1, length(ytest[,"time"])))
          #colnames(fold_test[["residuals"]]) = "residuals_residuals"
          colnames(fold_test[["residuals"]]) = "residuals"
        }

        rgcca_test = RGCCA::rgcca_transform(rgcca_train, fold_test)
        latent_space_test = Reduce(
          cbind,
          rgcca_test[names(rgcca_test)!="residuals"]
        )
        if(pars$clinical_fav) {
          latent_space_test = cbind(latent_space_test,
                                    as.matrix(clinicals[f]))
        }

        #fit = survival::survfit(glmnet_train, x=latent_space_train, y=ytrain,
                          #weights=glmnet_train$weights, newx=latent_space_test,
                          #se.fit=FALSE)
        # Computing CV measure
        if (pars$CV_measure == "cindex") {
          fit = predict(glmnet_train, newx=as.matrix(latent_space_test))
          res = apply(fit, 2, function(lp) survAUC::UnoC(ytrain, ytest, lp))
          #res2 = apply(fit, 2, glmnet::Cindex, y=ytest) # Harrell C index from glmnet
        } else if (pars$CV_measure == "ibrier") {
          fit = survival::survfit(glmnet_train, x=latent_space_train,
                                  y=ytrain, weights=glmnet_train$weights,
                                  newx=latent_space_test, se.fit=FALSE)
          probs = lapply(fit, getElement, name="surv")
          #time = lapply(fit, getElement, name="time")
          res = sapply(
              probs,
              function(p) 
                  SurvMetrics::IBS(ytest, t(p),
                                   IBSrange=c(min(ysurv[,"time"]),
                                              max(ysurv[,"time"])))
          )
        } else if (pars$CV_measure == "auc") {
          fit = predict(glmnet_train, newx=as.matrix(latent_space_test))
          res = apply(fit, 2, function(lp) survAUC::AUC.uno(ytrain, ytest, lp))
        }
        results = c(results, list(res))
        
      }

      results = Reduce(rbind, results)

      select_best = ifelse(pars$CV_measure %in% c("cindex", "auc"),
                           which.max,
                           which.min)
      mean_results = apply(results, 2, mean, na.rm=TRUE)
      best_lambda = lambdas[select_best(mean_results)]

      list(rgcca=fit_rgcca,
           glmnet=glmnet_fit,
           best_lambda=best_lambda,
           y=ysurv,
           x=latent_space)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags="predict")

      # get newdata and ensure same ordering in train and predict
      #newx = mlr3extralearners::ordered_features(task, self)
      newx = task$data()[,.SD, .SDcols=task$feature_names] # ensure target is not in data
      #newx = newx[, .SD, .SDcols=colnames(self$state$data_prototype)]

      block_list = lapply(
        pars$blocks,
        function(bl) newx[,
          .SD,
          .SDcols=names(newx) %like% paste0("_", bl, "$")
        ]
      )
      names(block_list) = pars$blocks

      block_list = lapply( # ensuring colnames respect rgcca conventions
        names(block_list),
        function(n) {
            ret = as.matrix(block_list[[n]])
            #colnames(ret) = paste0(n, "_", colnames(ret))
            ret
        }
      )
      names(block_list) = pars$blocks
      if(pars$supervised) {
        #null_mod = survival::coxph(ytest~1)
        block_list[["residuals"]] = as.matrix(rep(1, nrow(newx)))
        #colnames(block_list[["residuals"]]) = "residuals_residuals"
        colnames(block_list[["residuals"]]) = "residuals"
      }
      rgcca = RGCCA::rgcca_transform(self$model$rgcca, block_list)

      latent_space_new = Reduce(
        cbind,
        rgcca[names(rgcca)!="residuals"]
      )
      if(pars$clinical_fav) {
        clinicals = newx[, .SD, .SDcols=names(newx) %like% "clinical"]
        latent_space_new = cbind(latent_space_new,
                                 as.matrix(clinicals))
      }
      
      # Calculate predictions for the selected predict type.
      which_lambda = which(self$model$best_lambda==self$model$glmnet$lambda)
      fit = survival::survfit(self$model$glmnet,
                              s=self$model$best_lambda,
                              x=self$model$x,
                              y=self$model$y,
                              weights=self$model$glmnet$beta,
                              newx=latent_space_new,
                              se.fit=FALSE)
      lp = predict(self$model$glmnet,
                   newx=latent_space_new,
                   s=self$model$best_lambda)

      return(mlr3proba::.surv_return(times=fit$time,
                                     surv=t(fit$surv),
                                     lp=lp))

    }
  )
)

mlr3::mlr_learners$add("surv.rgcca", LearnerSurvrgcca)
