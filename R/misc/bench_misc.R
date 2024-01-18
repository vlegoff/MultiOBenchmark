suppressMessages({
    library(batchtools)
})

submitLrn <- function(lrn_id,
                      jobs_per_chunk=1,
                      resources=list(walltime=60*60, ncpus=1, memory=10000),
                      not_done=FALSE,
                      reg=getDefaultRegistry()) {

    ids = findNotSubmitted()
    if (not_done) ids = rbind(ids, findNotDone())
    ids = ids[!duplicated(ids)]
    err = findErrors()
    ids = ids[!(job.id %in% err$job.id)]
    #if (errors) ids = rbind(ids, findErrors())
    ids <- unwrap(getJobTable(ids, reg=reg))[
        learner_id==lrn_id,
        job.id
    ]
    chunks <- data.table(
        job.id=ids,
        chunk=batchtools::chunk(ids, chunk.size=jobs_per_chunk, shuffle=FALSE)
    )
    submitJobs(ids=chunks, resources=resources, reg=reg)

}

ifNotNA= function(e, default) {
    exp = enexpr(e)
    default = enexpr(default)
    val = rlang::`!!`(e)
    if (!is.na(val)) {return(val)} else {return(!!default)}
}
