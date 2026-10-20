library(tidyverse)
library(data.table)

ncpus = as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))
ncpus = ifelse(is.null(ncpus), 1, ncpus)
setDTthreads(ncpus)

gene_map = fread("vlegoff_gene_id_map.csv")
mirna_map = fread("vlegoff_mirna_gene_map.csv")
mirna_map[, `:=`(premat_ID=OriginalName, OriginalName=NULL)]

dup_symbols = gene_map[, .N, by="orig_symbol"][N>1][["orig_symbol"]]
dup_ids = gene_map[, .N, by="ensembl_gene_id"][N>1][["ensembl_gene_id"]]

cancers = c("LAML", "ESCA", "PAAD", "SARC", "LIHC", "COAD", "KIRP",
    "OV", "KIRC", "SKCM", "STAD", "BLCA", "UCEC", "LGG", "HNSC",
    "LUSC", "LUAD", "BRCA")

for(c in cancers) {

    print(c)

    dat = fread(paste0("../benchmark/data/", c, ".csv"))
    #dat[,ID:=1:nrow(dat)]

    dat_long = dat[,ID:=1:nrow(dat)] |>
        melt(id.vars=c("ID", "time", "status"))
    dat_long[,
        block:=sub("^.*_(clinical|cnv|mutation|rna|mirna)$", "\\1", variable)
    ]

    dat_clin = dat_long[block=="clinical"]
    dat_clin[, variable:=sub("_clinical$", "", variable)]

    dat_mut = dat_long[block=="mutation"]
    dat_mut = dat_mut[
        ,`:=`(mutation=value,
              orig_symbol=sub("_mutation$", "", variable),
              value=NULL,
              variable=NULL,
              block=NULL)
    ][
        !orig_symbol %in% dup_symbols
    ]
    dat_mut = gene_map[,.(orig_symbol, ensembl_gene_id)][
        dat_mut, on="orig_symbol"
    ]
    dat_mut[,orig_symbol:=NULL]

    dat_rna = dat_long[block=="rna"]
    dat_rna[
        ,`:=`(rna=value,
              ensembl_gene_id=sub("_rna$", "", variable),
              value=NULL,
              variable=NULL,
              block=NULL)
    ]

    dat_cnv = dat_long[block=="cnv"]
    dat_cnv[
        ,`:=`(cnv=value,
              ensembl_gene_id=sub("_cnv$", "", variable),
              value=NULL,
              variable=NULL,
              block=NULL)
    ]

    dat_mirna = dat_long[block=="mirna"]
    dat_mirna[
        ,`:=`(mirna=value,
              premat_ID=gsub("\\.", "-", sub("_mirna$", "", variable)),
              value=NULL,
              variable=NULL,
              block=NULL)
    ]
    dat_mirna = mirna_map[,.(premat_ID, target_ensembl)][
        dat_mirna, on="premat_ID", allow.cartesian=TRUE
    ]

    dat_mirna2 = dat_mirna[,
        mean(mirna),
        by=.(ID, time, status, target_ensembl)
    ][, `:=`(mirna=V1,
             V1=NULL,
             ensembl_gene_id=target_ensembl,
             target_ensembl=NULL)
    ]

    dat_merge = dat_mut[dat_rna, on=.(ID, time, status, ensembl_gene_id)]
    dat_merge[is.na(mutation), mutation:=0]
    dat_merge = dat_cnv[dat_merge, on=.(ID, time, status, ensembl_gene_id), nomatch=NULL]
    dat_merge = dat_mirna2[dat_merge, on=.(ID, time, status, ensembl_gene_id), nomatch=NULL]

    length(dat_merge[, unique(ensembl_gene_id)])
    length(dat_rna[, unique(ensembl_gene_id)])
    length(dat_cnv[, unique(ensembl_gene_id)])
    length(dat_mirna2[, unique(ensembl_gene_id)])
    length(dat_mut[, unique(ensembl_gene_id)])

    dat_merge_long = dat_merge |>
        melt(id.vars=c("ID", "time", "status", "ensembl_gene_id"),
             measure.vars=c("mutation", "cnv", "rna", "mirna"))
    dat_merge_long[, `:=`(block=variable, variable=ensembl_gene_id,
                          ensembl_gene_id=NULL)]
    dat_merge_long = rbind(dat_merge_long, dat_clin)
    dat_merge_long[, variable:=paste0(variable, "_", block)]
    dat_merge_long[, block:=NULL]
    dat_new = dcast(dat_merge_long, ID+time+status~variable, value.var="value")
    dat_new = dat_new[order(ID)]
    dat_new[, ID:=NULL]
    #dat_new = dat_new[order(time)]  #JE SUIS DEBILE

    fwrite(dat_new, file=paste0("tensor/vlegoff_", c, "_tensor.csv"))

}
