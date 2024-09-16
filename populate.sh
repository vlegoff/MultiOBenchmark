#!/bin/bash
#MSUB -q normal
#MSUB -n 1
#MSUB -c 8
#MSUB -E --mem-per-cpu=10G
#MSUB -T 7200
#MSUB -o logs/job.%I.out
#MSUB -e logs/job.%I.err

module load extenv/own
module load benchbatch

REG=test

benchbatch populate \
    -l learners/$REG.conf.R \
    -r registries/$REG \
    -d TCGA18.conf.R \
    -N $SLURM_JOB_CPUS_PER_NODE \
    -s 1234
