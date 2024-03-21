suppressMessages({
    library(batchtools)
})

submitLrn <- function(lrn_id,
                      jobs_per_chunk=1,
                      resources=list(walltime=60*60, ncpus=1, memory=10000),
                      resubmit_not_finished=FALSE,
                      resubmit_errors=FALSE,
                      reg=getDefaultRegistry()) {

    ids = findNotSubmitted()
    print(ids)
    err =  findErrors()
    not_finished = findNotDone()[
        !(job.id %in% ids$job.id | job.id %in% err$job.id)
    ]
    if (resubmit_errors) ids = rbind(ids, err)
    if (resubmit_not_finished) ids = rbind(ids, not_finished)

    #ids = findNotSubmitted()
    #if (not_done) ids = rbind(ids, findNotDone())
    #ids = ids[!duplicated(ids)]
    #if (!resubmit_errors) {
        #err = findErrors()
        #ids = ids[!(job.id %in% err$job.id)]
    #}
    #if (errors) ids = rbind(ids, findErrors())
    if(nrow(ids)==0) {stop("nothing to run")}
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
