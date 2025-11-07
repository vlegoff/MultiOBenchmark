library(mlr3proba)

measures = list(
    msr("surv.cindex", weight_meth="G2", p_max=0.9, id="cindex_p0.9"), # uno C
    msr("surv.cindex", weight_meth="G2", id="cindex_default"), # uno C
    msr("surv.graf", id="graf"),
    msr("surv.graf", p_max=0.9, id="graf_p0.9"),
    msr("surv.graf", p_max=0.8, id="graf_p0.8"),
    msr("surv.graf", p_max=0.75, id="graf_p0.75"),
    msr("surv.rcll"),
    msr("surv.dcalib"),
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
