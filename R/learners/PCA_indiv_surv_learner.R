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

LearnerSurvPCAI = R6Class("LearnerSurvPCAI",
  inherit = mlr3proba::LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        blocks=p_uty(tags=c("train", "predict")),
        clinical_fav=p_lgl(default=TRUE, tags=c("train", "predict")),
        rankA=p_int(0L, default=1L, tags=c("train", "predict")),
        nfolds=p_int(1L, default=10L, tags=c("train")),
        CV_measure=p_fct(c("cindex", "ibrier", "auc"), default="cindex",
                          tags=c("train")),
        seed=p_int(0L, special_vals=list(NULL), default=NULL, tags=c("train"))
      )
      param_set$values$rankA=1L
      param_set$values$clinical_fav = TRUE
      param_set$values$nfolds = 10L
      param_set$values$CV_measure = "cindex"
      param_set$values$seed = NULL

      super$initialize(
        id = "surv.rgcca",
        packages = c("r.jive", "glmnet", "survival", "survAUC", "SurvMetrics",
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

      set.seed(pars$seed)

      # Some checks

      # clinical must not be present in the blocks
      if (("clinical" %in% pars$blocks) & pars$clinical_fav)
        stop("Clinical cannot be in both blocks and latent space")

      # ncomp must be inferior or equal to smallest block

      # Get blocks
      block_list = lapply(
        pars$blocks,
        function(bl) x[, .SD, .SDcols=names(x) %like% paste0("_", bl, "$")]
      )
      names(block_list) <- pars$blocks

      # Compute rgcca
      pca_args = list(
        center=TRUE,
        scale=TRUE,
        scannf=FALSE,
        nf=pars$rankA
      )
    
      fit_pca = lapply(block_list,
        function(x) do.call(ade4::dudi.pca, c(list(df=x), pca_args)))
      latent_space = Reduce(cbind, lapply(fit_pca, `[[`, "li"))
      names(latent_space) = c()
      latent_space = as.matrix(latent_space)

      if (pars$clinical_fav) {
        clinicals = x[, .SD, .SDcols=names(x) %like% "clinical"]
        latent_space = cbind(latent_space, as.matrix(clinicals))
      }

      # implement case where there is 1 PC and no clinical data
      # it corresponds to a simple coxph model, no need for CV
      # if (ncol(latent_space)) return(coxph) 

      #Fit glmnet to get lambdas
      glmnet_fit = glmnet::glmnet(latent_space, ysurv, family="cox", alpha=0,
                                  nlambda=100)
      lambdas = glmnet_fit$lambda

      folds = caret::createFolds(ysurv[, "status"], k=pars$nfolds, list=TRUE)

      results = list()
      for (f in folds) {
        
          print(f)

        #train
        ytrain = ysurv[-f]
        fold_train = lapply(block_list, function(x) x[-f])

        pca_train = lapply(fold_train,
          function(x) do.call(ade4::dudi.pca, c(list(df=x), pca_args)))
        latent_space_train = Reduce(cbind, lapply(pca_train, `[[`, "li"))
        names(latent_space_train) = c()
        latent_space_train = as.matrix(latent_space_train)

        if(pars$clinical_fav) {
          latent_space_train = cbind(latent_space_train,
                                     as.matrix(clinicals[-f]))
        }
        
        glmnet_train = glmnet::glmnet(latent_space_train, ytrain,
                                      family="cox", alpha=0, lambda=lambdas)

        #test
        ytest = ysurv[f]
        fold_test = lapply(block_list, function(x) x[f])

        pca_test = lapply(seq_along(fold_test),
          function(i) ade4::suprow(pca_train[[i]], fold_test[[i]]))
        latent_space_test = Reduce(cbind, lapply(pca_test, `[[`, "lisup"))
        names(latent_space_test) = c()
        latent_space_test = as.matrix(latent_space_test)

        if(pars$clinical_fav) {
          latent_space_test = cbind(latent_space_test,
                                    as.matrix(clinicals[f]))
        }

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

      select_best = ifelse(pars$CV_measure %in% c("cindex, auc"),
                           which.max,
                           which.min)
      mean_results = apply(results, 2, mean, na.rm=TRUE)
      best_lambda = lambdas[select_best(mean_results)]

      list(pca=fit_pca,
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
        function(bl) newx[, .SD, .SDcols=names(newx) %like% paste0("_", bl, "$")]
      )
      names(block_list) = pars$blocks

      pca = lapply(seq_along(block_list),
        function(i) ade4::suprow(self$model$pca[[i]], block_list[[i]]))
      latent_space_new = Reduce(cbind, lapply(pca, `[[`, "lisup"))
      names(latent_space_new) = c()
      latent_space_new = as.matrix(latent_space_new)

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

mlr3::mlr_learners$add("surv.pca_indiv", LearnerSurvPCAI)
