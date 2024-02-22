#!/bin/bash
#SBATCH -p normal
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --mem-per-cpu=10G
#SBATCH -t 04:00:00
#SBATCH -o slurm_logs/job.%j.out
#SBATCH -e slurm_logs/job.%j.err

module unload r && module load r/4

Rscript R/extract_results.R -r ${1} -N $SLURM_JOB_CPUS_PER_NODE \
    -C 100 -d results
