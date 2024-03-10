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

LearnerSurvjive = R6Class("LearnerSurvjive",
  inherit = mlr3proba::LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        blocks=p_uty(tags=c("train", "predict")),
        clinical_fav=p_lgl(default=TRUE, tags=c("train", "predict")),
        rankJ=p_int(0L, default=1L, tags=c("train", "predict")),
        rankA=p_int(0L, default=1L, tags=c("train", "predict")),
        nfolds=p_int(1L, default=10L, tags=c("train")),
        CV_measure=p_fct(c("cindex", "ibrier", "auc"), default="cindex",
                          tags=c("train")),
        seed=p_int(0L, special_vals=list(NULL), default=NULL, tags=c("train"))
      )
      param_set$values$rankJ=1L
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
        function(bl) t(x[, .SD, .SDcols=names(x) %like% paste0("_", bl, "$")])
      )
      names(block_list) <- pars$blocks

      # Compute rgcca
      jive_args = list(
        rankJ=pars$rankJ,
        rankA=rep(pars$rankA, length(block_list)),
        method="given",
        orthIndiv=FALSE,
        scale=TRUE,
        center=TRUE
      )
    
      fit_jive = do.call(r.jive::jive,
                         c(list(data=lapply(block_list, as.matrix)),
                           jive_args))

      predict = r.jive::jive.predict(data.new=lapply(block_list, as.matrix),
        fit_jive)

      latent_space = matrix(0,
        nrow=nrow(x),
        ncol=pars$rankJ+length(block_list)*pars$rankA)

      #if (pars$rankJ > 0) {
      #  joint = Reduce(cbind, lapply(fit_jive$joint, t))
      #  joint_SVD = svd(joint)
      #  joint_PCs = joint_SVD$u[,1:pars$rankJ] %*%
      #    diag(joint_SVD$d[1:pars$rankJ], ncol=pars$rankJ)
      #  joint_weights = joint_SVD$v[,pars$rankJ]
      #  latent_space[,1:pars$rankJ] = joint_PCs
      #}

      ##ncomp = length(block_list) - supervised # rename to nblocks
      ##indiv = lapply(1:ncomp, function(i) t(fit_jive$individual[[i]])) 
      #if (pars$rankA > 0) {
      #  indiv = lapply(seq_along(block_list),
      #    function(i) t(fit_jive$individual[[i]])) 
      #  indiv_SVD = lapply(indiv, svd)
      #  indiv_PCs = sapply(
      #    seq_along(block_list),
      #    function(i) {
      #      print(i)
      #      indiv_SVD[[i]]$u[,1:fit_jive$rankA[i]] %*%
      #        diag(indiv_SVD[[i]]$d[1:fit_jive$rankA[i]],
      #             ncol=fit_jive$rankA[i])
      #      }
      #  )
      #  latent_space[,(pars$rankJ+1):ncol(latent_space)] = indiv_PCs
      #}

      if(pars$rankJ > 0) {
        latent_space[,1:pars$rankJ] = t(predict$joint.scores)
      }
      
      if(pars$rankA > 0) {
        latent_space[,(pars$rankJ+1):ncol(latent_space)] = 
          t(Reduce(rbind, predict$indiv.scores)) # cbind was not working
      }

 #     latent_space = Reduce(cbind,
 #       c(if (rankJ!=0) list(t(pred$joint.scores)),
 #         if (any(rankA!=0)) lapply(pred$indiv.scores, t))
 #     )

      if (pars$clinical_fav) {
        clinicals = x[, .SD, .SDcols=names(x) %like% "clinical"]
        latent_space = cbind(latent_space, as.matrix(clinicals))
      }

      #Fit glmnet to get lambdas
      glmnet_fit = glmnet::glmnet(latent_space, ysurv, family="cox", alpha=0,
                                  nlambda=100)
      lambdas = glmnet_fit$lambda

      folds = caret::createFolds(ysurv[, "status"], k=pars$nfolds, list=TRUE)

      results = list()
      for (f in folds) {

        #train
        ytrain = ysurv[-f]
        fold_train = lapply(block_list, function(x) as.matrix(x[,-f]))

        jive_train = do.call(r.jive::jive, c(list(data=fold_train), jive_args))

        #latent_space_train = Reduce(cbind, c(list(t(pred_train$joint.scores)),
          #lapply(pred_train$indiv.scores, t)))

        latent_space_train = matrix(0,
          nrow=nrow(x)-length(f),
          ncol=pars$rankJ+length(block_list)*pars$rankA)

        nweights = sum(sapply(fold_train, nrow))
        weights_train = matrix(0,
        nrow=nweights, ncol=pars$rankJ+length(block_list)*pars$rankA)

        #if (pars$rankJ > 0) {
        #  joint = Reduce(cbind, lapply(jive_train$joint, t))
        #  joint_SVD = svd(joint)
        #  joint_PCs = joint_SVD$u[,1:pars$rankJ] %*%
        #    diag(joint_SVD$d[1:pars$rankJ], ncol=pars$rankJ)
        #  latent_space_train[,1:pars$rankJ] = joint_PCs
        #  weights_train[,1:pars$rankJ] = joint_SVD$v[,1:pars$rankJ]
        #}

        ##ncomp = length(block_list) - supervised # rename to nblocks
        ##indiv = lapply(1:ncomp, function(i) t(fit_jive$individual[[i]])) 
        #if (pars$rankA > 0) {
        #  indiv = lapply(seq_along(block_list),
        #    function(i) t(jive_train$individual[[i]])) 
        #  indiv_SVD = lapply(indiv, svd)
        #  indiv_PCs = sapply(
        #    seq_along(block_list),
        #    function(i) {
        #      print(i)
        #      indiv_SVD[[i]]$u[,1:jive_train$rankA[i]] %*%
        #        diag(indiv_SVD[[i]]$d[1:jive_train$rankA[i]],
        #             ncol=jive_train$rankA[i])
        #      }
        #  )
        #  latent_space_train[,(pars$rankJ+1):ncol(latent_space_train)] =
        #    indiv_PCs
        #  weights_train[,(pars$rankJ+1):ncol(latent_space_train)] =
        #    sapply(seq_along(block_list),
        #      function(i) indiv_SVD[[i]]$v[,1:pars$rankA])
        #}

        train_pred = r.jive::jive.predict(fold_train, jive_train)

        latent_space_train = matrix(0,
          nrow=nrow(x)-length(f),
          ncol=pars$rankJ+length(block_list)*pars$rankA)

        if(pars$rankJ > 0) {
          latent_space_train[,1:pars$rankJ] = t(train_pred$joint.scores)
        }
        
        if(pars$rankA > 0) {
          latent_space_train[,(pars$rankJ+1):ncol(latent_space_train)] = 
            t(Reduce(rbind, train_pred$indiv.scores)) # cbind was not working
        }

        if(pars$clinical_fav) {
          latent_space_train = cbind(latent_space_train,
                                     as.matrix(clinicals[-f]))
        }
        
        glmnet_train = glmnet::glmnet(latent_space_train, ytrain,
                                      family="cox", alpha=0, lambda=lambdas)

        #test
        ytest = ysurv[f]
        fold_test = lapply( # ensuring colnames respect rgcca conventions
          names(block_list),
          function(n) {
            block_list[[n]][,f]
          }
        )
        names(fold_test) = names(block_list)

        jive_test = r.jive::jive.predict(fold_test, jive_train)

        latent_space_test = matrix(0,
          nrow=length(f), ncol=pars$rankJ+length(block_list)*pars$rankA)

        if(pars$rankJ > 0) {
          latent_space_test[,1:pars$rankJ] = t(jive_test$joint.scores)
        }
        
        if(pars$rankA > 0) {
          latent_space_test[,(pars$rankJ+1):ncol(latent_space_test)] = 
            t(Reduce(rbind, jive_test$indiv.scores)) # cbind was not working
        }

        if(pars$clinical_fav) {
          latent_space_test = cbind(latent_space_test,
                                    as.matrix(clinicals[f]))
        }

        #fit = survival::survfit(glmnet_train, x=latent_space_train, y=ytrain,
                          #weights=glmnet_train$weights, newx=latent_space_test,
                          #se.fit=FALSE)
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

      list(jive=fit_jive,
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

      block_list = lapply(
        names(block_list),
        function(n) {
            t(as.matrix(block_list[[n]]))
        }
      )
      names(block_list) = pars$blocks

      jive = r.jive::jive.predict(block_list, self$model$jive)

      latent_space_new = matrix(0,
        nrow=nrow(newx),
        ncol=self$model$jive$rankJ+length(block_list)*self$model$jive$rankA)

      if(self$model$jive$rankJ > 0) {
        latent_space_new[,1:pars$rankJ] = t(jive$joint.scores)
      }
      
      if(pars$rankA > 0) {
        latent_space_new[,(pars$rankJ+1):ncol(latent_space_new)] = 
          t(Reduce(rbind, jive$indiv.scores)) # cbind was not working
      }

      #latent_space_new = Reduce(cbind,
                                #c(list(t(jive$joint.score)),
                                  #lapply(jive$indiv.score, t)))

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

mlr3::mlr_learners$add("surv.jive", LearnerSurvjive)
