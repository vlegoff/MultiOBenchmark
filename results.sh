#!/bin/bash
#MSUB -q normal
#MSUB -n 1
#MSUB -c 8
#MSUB -E --mem-per-cpu=10G
#MSUB -E -t 24:00:00
#MSUB -o logs/job.%I.out
#MSUB -e logs/job.%I.err

module load extenv/own
module load benchbatch

REG=test

mkdir results/$REG

benchbatch results \
    -r registries/$REG \
    -d results/$REG \
    -m measures.conf.R \
    -N $SLURM_JOB_CPUS_PER_NODE \
    -C 100 -v
