#!/bin/bash
#MSUB -q normal
#MSUB -n 1
#MSUB -c 8
#MSUB -E --mem-per-cpu=10G
#MSUB -T 14400
#MSUB -o logs/job.%I.out
#MSUB -e logs/job.%I.err

# to replace custom module
ENV=env/inti_el9

module load micromamba
micromamba activate -p $ENV

Rscript R/merge_results.R \
    -f results \
    -d results \
    -N $SLURM_JOB_CPUS_PER_NODE \
    -m measures.conf.R
