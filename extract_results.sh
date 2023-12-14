#!/bin/bash
#SBATCH -p normal
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --mem-per-cpu=10G
#SBATCH -t 02:00:00

module unload r && module load r/4

Rscript R/extract_results.R -r ${1} -N $SLURM_JOB_CPUS_PER_NODE -C 100 -d results
