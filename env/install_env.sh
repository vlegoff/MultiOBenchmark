CONDA_NAME="inti_el9"

micromamba env create -p ./$CONDA_NAME --file benchmark_env_nobuild.yaml
micromamba activate -p ./$CONDA_NAME

cat << EOF > package_install.R
    packages_cran <- c(
        "mlr3batchmark",
        "blockForest",
        "ipflasso",
        "NMF",
        "pec",
        "prioritylasso",
        "r.jive",
        "riskRegression",
        "survAUC",
        "SurvMetrics",
        "tensorBSS",
        "coefplot"
    )
    packages_github <- c(
        "enorthrop/sup.r.jive",
        "vlegoff/IntNMF",
        "vlegoff/benchbatch",
        "vlegoff/survjdr",
        "vlegoff/iClusterPlus"
    )
    install.packages(packages_cran, repos="https://pbil.univ-lyon1.fr/CRAN")
    devtools::install_github(packages_github, upgrade_dependencies=FALSE)
    # need version 0.8.2 of mlr3proba
    mlr3verse = c(
        "mlr3",
        "mlr3batchmark",
        "mlr3cluster",
        "mlr3cmprsk",
        "mlr3data",
        "mlr3extralearners",
        "mlr3filters",
        "mlr3fselect",
        "mlr3hyperband",
        "mlr3inferr",
        "mlr3learners",
        "mlr3mbo",
        "mlr3measures",
        "mlr3misc",
        "mlr3pipelines",
        "mlr3proba",
        "mlr3tuning",
        "mlr3tuningspace",
        "mlr3verse",
        "mlr3viz"
    )
    install.packages(mlr3verse, repos="https://mlr-org.r-universe.dev")
EOF

Rscript package_install.R
rm package_install.R
