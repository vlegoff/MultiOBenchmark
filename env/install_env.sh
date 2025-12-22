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
        "tensorBSS"
    )
    packages_github <- c(
        "enorthrop/sup.r.jive",
        "vlegoff/IntNMF",
        "vlegoff/benchbatch",
        "vlegoff/survjdr",
        "vlegoff/iClusterPlus"
    )
    install.packages(packages_cran, repos="https://pbil.univ-lyon1.fr/CRAN")
    devtools::install_github(packages_github)
EOF

Rscript package_install.R
rm package_install.R
