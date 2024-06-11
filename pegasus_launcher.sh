#!/bin/bash
#SBATCH -p normal
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --mem-per-cpu=10G
#SBATCH -t 24:00:00
#SBATCH -o logs/job.%j.out
#SBATCH -e logs/job.%j.err

Rscript R/bench_experiment.R \
    -r registries/${1} \
    -l configs/${1}.conf.R \
    -d TCGA18.conf.R \
    -c resources.conf.R \
    -b batchtools_interactive.conf.R \
    -N 4 -s 1234 \
    --resubmit_not_finished \
    --resubmit_errors \
    --skip_batchmark \
    --pegasus
