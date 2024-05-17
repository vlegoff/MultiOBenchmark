#!/bin/bash
#SBATCH -p normal
#SBATCH -n 1
#SBATCH -c 8
#SBATCH --mem-per-cpu=10G
#SBATCH -t 04:00:00
#SBATCH -o logs/job.%j.out
#SBATCH -e logs/job.%j.err

Rscript R/extract_results.R \
    -r registries/${1} \
    -d results/${1}
    -m measures.conf.R \
    -N $SLURM_JOB_CPUS_PER_NODE \
    -C 100
