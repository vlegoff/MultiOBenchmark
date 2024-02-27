cluster.functions =
    makeClusterFunctionsSlurm(template="hpc_templates/slurm-simple.tmpl",
                              array.jobs=TRUE, nodename="inti.cnrgh.fr")
packages=c("mlr3", "mlr3proba", "data.table")
