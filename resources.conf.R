#resources = list(
    #"pegasus"=list(walltime=60*60*24, ntasks=50, mem="10G", log="logs")
#)
resources = list(array.job=TRUE, n.chunks=20, walltime=60*60*2, memory="10G",
    ncpus=1)
