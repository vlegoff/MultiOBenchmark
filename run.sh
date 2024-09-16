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

benchbatch run \
    -m pegasus \
    -c resources/pegasus.conf.R \
    -r registries/$REG \
    -l configs/${1}.conf.R \
    -N 4 \
    --resubmit_not_finished \
    --resubmit_errors
