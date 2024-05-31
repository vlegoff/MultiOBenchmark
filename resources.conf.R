resources = list(
    "pegasus"=list(walltime=60*60*24, ntasks=200, mem="10G", log="logs"),
    "kaplan"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50,
        chunks.as.arrayjobs=TRUE
    ),
    "clin_ref"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50,
        chunks.as.arrayjobs=TRUE
    ),
    "glmnet_clin"=list(
        walltime=60*60*24, ncpus=1, memory=10000, jobs_per_chunk=50
    )
)
