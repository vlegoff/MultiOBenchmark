important_packages = c(
    "data.table",
    "pec",
    "riskRegression",
    "survival",
    "tidyverse",
    "mlr3verse",
    "mlr3proba",
    "mlr3extralearners",
    "mlr3batchmark",
    "RGCCA",
    "r.jive",
    "sup.r.jive",
    "argparse",
    "batchtools",
    "parallel",
    "ipflasso",
    "prioritylasso",
    "ade4",
    "blockForest",
    "mboost",
    "pracma",
    "survAUC",
    "SurvMetrics",
    "caret",
    "glmnet",
    "randomForest",
    "randomForestSRC",
    "foreach",
    "doParallel",
    "ranger"
)

for (p in important_packages) {
    print(p)
    if(!require(p, character.only=TRUE)) install.packages(p)
}
