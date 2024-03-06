
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

source("R/learners/learner_seq_mod_surv.R")

LearnerSurvRGCCA = R6Class("LearnerSurvRGCCA",
  inherit = LearnerSeqMod,
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
        nlambdas=p_int(10L, 1000L, default=100L, tags=c("train")),
        CV_measure=p_fct(c("cindex", "ibrier", "auc"), default="cindex",
                          tags=c("train")),
        seed=p_int(0L, special_vals=list(NULL), default=NULL, tags=c("train"))
      )
      param_set$values = param_set$default

      super$initialize(
        id = "surv.rgcca2",
        packages = c("RGCCA", "mlr3misc"),
        feature_types = c("integer", "numeric", "factor"),
        predict_types = c("crank", "lp", "distr"),
        param_set = param_set,
        properties = c(),
        man = "",
        label = ""
      )
    }
  ),
  private = list(

    rgcca_args = NULL,

    train_jdr = function(x, y, pars) {
        
        if(is.null(private$rgcca_args)) {
            if(!pars$supervised) {
                complete_matrix = matrix(1, length(pars$blocks),
                                       length(pars$blocks))
                diag(complete_matrix) = 0
                rownames(complete_matrix) = pars$blocks
                colnames(complete_matrix) = pars$blocks
            }

            private$rgcca_args = list(
                response=if(pars$supervised) length(x) + 1,
                connection=if(!pars$supervised) complete_matrix,
                tau=c(rep(pars$tau, length(x)),
                      pars$tau*(!pars$supervised)),
                ncomp=pars$ncomp,
                scheme=pars$scheme,
                method="rgcca",
                scale=TRUE,
                scale_block="inertia",
                verbose=F
            )
        }

        if(pars$supervised) {
            null_mod = survival::coxph(y~1)
            x[["residuals"]] = residuals(null_mod, type="deviance")
        }

        rgcca_fit = mlr3misc::invoke(RGCCA::rgcca,
            .args=c(list(blocks=x), private$pca_args))
        comps = Reduce(cbind,
            rgcca_fit$Y[names(rgcca_fit$Y)!="residuals"])
        return(list(x=comps, jdr=rgcca_fit))
    },

    predict_jdr = function(newx, jdr, pars) {
        if(pars$supervised) {
            newx[["residuals"]] = as.matrix(rep(1, nrow(newx[[1]])))
            colnames(newx[["residuals"]]) = "residuals"
        }
        rgcca_pred = RGCCA::rgcca_transform(jdr, newx)
        pred_space = Reduce(cbind, rgcca_pred[names(rgcca_pred)!="residuals"])
        return(list(x=pred_space))
    }

  )
)

mlr3::mlr_learners$add("surv.rgcca2", LearnerSurvRGCCA)
