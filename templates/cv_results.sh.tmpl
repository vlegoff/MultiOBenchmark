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

Rscript R/cv_results.R \
    -f results/cv_logs/$REG \
    -c data/folds.rds \
    -d results/cv_logs/$REG \
    -N $SLURM_JOB_CPUS_PER_NODE

tar -czvf results/cv_logs/$REG/cv_logs.tar.gz results/cv_logs/$REG/*.rds \
    --remove-files
