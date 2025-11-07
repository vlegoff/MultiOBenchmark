#!/bin/bash
#MSUB -q normal
#MSUB -n 1
#MSUB -c 8
#MSUB -E --mem-per-cpu=10G
#MSUB -T 7200
#MSUB -o logs/job.%I.out
#MSUB -e logs/job.%I.err

module load micromamba
micromamba activate -p env/r4.5.2

module load extenv/own
module load benchbatch

REG=mcia_test

benchbatch run \
    -m pegasus \
    -c resources/pegasus.conf.R \
    -r registries/$REG \
    -N $SLURM_JOB_CPUS_PER_NODE \
    --resubmit_not_finished #\
    #--resubmit_errors
