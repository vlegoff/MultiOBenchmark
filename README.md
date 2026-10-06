# MultiOBenchmark

This repositery contains the scripts used to run the benchmark presented in X.
It is focused on measuring the performance of joint dimension reduction (JDR)
for survival prediction from multi-omics data, but could in theory be reused
for other tasks, methods and data.

## Software installation

The code presented here relies heavily on the mlr3 R framework. It depends on
2 custom packages:

    - benchbatch, which wraps around mlr3 and batchtools to run an mlr3
    benchmark on a High-Performance Computing (HPC) setting
    - survjdr which implemends JDR methods as mlr3 learners

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

Running the benchmark is done in 4 main steps:

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

### Learner config files

### Data config files

### Measures config files

### Scripts

### HPC consideration


