# MultiOBenchmark

This repositery contains the scripts used to run the benchmark presented in X.
It is focused on measuring the performance of joint dimension reduction (JDR)
for survival prediction from multi-omics data, but could in theory be reused
for other tasks, methods and data.q

Code used to run bootstrap experiments for interpretability are available at 
[vlegoff/JDR_bootstrap](https://github.com/vlegoff/JDR_bootstrap).

## Software installation

The code presented here relies heavily on the mlr3 R framework. It depends on
2 custom packages:

- [benchbatch](https://github.com/vlegoff/benchbatch), which wraps around mlr3 and batchtools to run an mlr3
benchmark on a High-Performance Computing (HPC) setting
- [survjdr](https://github.com/vlegoff/survjdr) which implemends JDR methods as mlr3 learners

These custom packages, the mlr3 framework, the JDR methods and all necessary
dependencies are contained in a conda environment which can be installed by
running the script `env/install_env.sh`

## Input data

Input data has been (will be) deposed on Zenodo, both the original datasets 
and the tensorized datasets.

### Tensors construction

If you wish to rebuild the tensors, you first have to run the scripts
`data/vlegoff_gene_map.R` and `vlegoff_mirna.R` which respectively construct 
correspondance files for mapping gene symbols to ensembl gene ids and for
mapping miRNAs to their targets. From these files as well as the non-tensorized
data, running the script `data/vlegoff_create_tensor.R` creates the csv files
for the tensorized datasets.

## Running the benchmark

Running the benchmark is done in 3 main steps:

- the populate step: a batchtools registry is created to store the jobs 
to run and "populated" with the appropriate jobs, as described in config 
files. Each (outer) resampling of a learner counts as a job.
- the run step: jobs from the registry created during the populate step are
run in parallel, either on separate cores of a single machine or, more
realistically, on multiple machines of an HPC setup.
- the results step: performance metrics are computed for the finished jobs
of a registry and gathered into a .csv file. For dimension reduction only,
results from the inner cross-validation are gathered into .csv file.

Finally, the `merge_results.sh` script gather all results into a clean set of
csv files.

### Examples

Two examples on a small subset are provided: 

- `example_cox.sh` which run references methods on clinical data and LASSO on 
multi-omics data
- `example_RGCCA.sh` which runs supervised RGCCA

These two examples are made to run on a local machine using 8 cores.

Breaking the steps of the second example, we get the populate step

```
Rscript R/populate.R \
    -l configs/learners/example_RGCCA.conf.R \
    -r registries/example_RGCCA \
    -d configs/data/TCGA_example.conf.R \
    -N 8 \
    -s 1234
```

The learners to run are stored in a config file (here 
`configs/learner/example_RGCCA.conf.R`), which is a R file and should contains
a list named `learners` of mlr3 learners to run.

The datasets on which to run these learners are also defined in a config file
(here `configs/data/TCGA_example.conf.R`) which contains information about the
resamplings and dataset. It should contain a variable named `cv` containing
a valid mlr3 resampling objet, a `task_type` variable defining the type of task,
and a `datasets` list where each element define a dataset whith the path to the
corresponding csv file, target variables for the task and optionnally strata
for resampling.

```
cv = rsmp("cv", folds=5)

task_type="survival" # or "regression", "classification"

datasets = list(
    "COAD"=list(
        path="data/herrmann2021/COAD.csv",
        target=c("time", "status"),
        strata=c("status")
    ),
    "KIRP"=list(
        path="data/herrmann2021/KIRP.csv",
        target=c("time", "status"),
        strata=c("status")
    )
)
```

Alternatively, a custom resampling can be provided, if a named list of folds
for each dataset is provided.

```
cv = rsmp("custom")
folds = readRDS("data/folds.rds") # folds used in the article
paired_cv = TRUE # to ensure each resampling is attributed to the corresponding dataset
datasets = list(...)
```

The run step launches the jobs defined by these configuration files

```
Rscript R/run.R \
    -m multicore \
    -r registries/example_RGCCA \ # registry created by the populate step
    -N 8 \
    --resubmit_not_finished 
```

The results computes the appropriate performance metrics:

```
Rscript R/results.R \
    -r registries/example_RGCCA \
    -d results/example_RGCCA \
    -m measures.conf.R \
    -N 8 \
    -C 100 -v
```

Performance metrics are stored in a config file defining a `measures` list
containing valid mlr3 measure objects:

```
measures = list(
    msr("surv.cindex", weight_meth="G2", p_max=0.9, id="cindex_p0.9"),
    msr("surv.graf", p_max=0.9, id="graf_p0.9"),
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
```

Finally, for DR methods only, the cv\_results step gather the inner 
cross-validation metrics and cox model weights:

```
Rscript R/cv_results.R \
    -f results/cv_logs/example_RGCCA \
    -c data/folds.rds \
    -d results/cv_logs/example_RGCCA \
    -N 8
```

### HPC consideration

Ideally, this should be run on an HPC setup. In such a setup, it is more
convenient to run each step with a separate script. Templates for such scripts
are provided in the `templates` folder. The major difference is for the run 
step, which can take an argument indicating which scheduler to use, as well as 
a config files defining scheduler-specific variables (e.g. number of cores to use).
See for instance `configs/resources/pegasus.conf.R.tmpl` and 
`configs/resources/slurm.conf.R.tmpl`.

Note that the templates provided are adapted to our local HPC cluster and will
necessite adaptations to run on another cluster. Most notably, these scripts
rely on an uncommon scheduler and the run step requires the availability of the 
pegasus microscheduler, but can be adapted to run with slurm.
