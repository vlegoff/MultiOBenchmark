#!/bin/bash
#MSUB -q normal
#MSUB -n 1
#MSUB -c 8
#MSUB -E --mem-per-cpu=10G
#MSUB -T 14400
#MSUB -o logs/job.%I.out
#MSUB -e logs/job.%I.err

#module load extenv/own
#module load benchbatch

# to replace custom module
ENV=env/inti_el9
REG=JIVE_woClin

module load micromamba
micromamba activate -p $ENV

mkdir results/$REG

Rscript R/results.R \
    -r registries/$REG \
    -d results/$REG \
    -m measures.conf.R \
    -N $SLURM_JOB_CPUS_PER_NODE \
    -C 100 -v
