measures = list(
    msr("surv.cindex", weight_meth="G2", p_max=0.9), # uno C
    msr("surv.cindex", weight_meth="G2", id="cindex_default"), # uno C
    msr("surv.graf", id="graf_default"),
    msr("surv.graf", p_max=0.9), # ibrier
    msr("surv.rcll"),
    msr("surv.dcalib"),
    msr("time_train"),
    msr("time_predict"),
    msr("time_both")
)
