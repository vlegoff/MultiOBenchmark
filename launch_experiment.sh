#!/bin/bash
#SBATCH -p normal
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --mem-per-cpu=10G
#SBATCH -t 02:00:00
#SBATCH -o slurm_logs/job.%j.out
#SBATCH -e slurm_logs/job.%j.err

module unload r && module load r/4.1

Rscript R/bench_experiment.R -r ${1} -b batchtools.conf.R \
    -N $SLURM_JOB_CPUS_PER_NODE -s 1234
