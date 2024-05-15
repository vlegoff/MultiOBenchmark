#!/bin/bash
#SBATCH -p normal
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --mem-per-cpu=10G
#SBATCH -t 02:00:00
#SBATCH -o logs/job.%j.out
#SBATCH -e logs/job.%j.err

Rscript R/bench_experiment.R -r ${1} -b batchtools_slurm.conf.R \
    -N $SLURM_JOB_CPUS_PER_NODE -s 1234 --resubmit_not_finished
