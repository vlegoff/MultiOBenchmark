#!/bin/bash
#SBATCH -p normal
#SBATCH -n 21
#SBATCH -c 1
#SBATCH --mem-per-cpu=10G
#SBATCH -t 01:00:00
#SBATCH -o slurm_logs/job.%j.out
#SBATCH -e slurm_logs/job.%j.err

module unload r && module load r/4.1
module load pegasus

Rscript R/bench_experiment.R -r ${1} -b batchtools_interactive.conf.R \
    -N $SLURM_JOB_CPUS_PER_NODE -s 1234 --pegasus

srun -n $((SLURM_NTASKS)) -c 1 \
    pegasus-mpi-cluster -m 1 registries/${1}/pegasus/tasks.dag
