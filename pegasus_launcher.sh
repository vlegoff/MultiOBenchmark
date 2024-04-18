#!/bin/bash
#SBATCH -p normal
#SBATCH -n 4
#SBATCH -c 1
#SBATCH --mem-per-cpu=10G
#SBATCH -t 24:00:00
#SBATCH -o logs/job.%j.out
#SBATCH -e logs/job.%j.err

Rscript R/bench_experiment.R -r ${1} -b batchtools_interactive.conf.R \
    -N 4 -s 1234 --pegasus --resubmit_not_finished
