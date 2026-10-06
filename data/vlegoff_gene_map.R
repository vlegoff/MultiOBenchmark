library(biomaRt)

# first we get all symbols & ids from all datasets
cancers = c("LAML", "ESCA", "PAAD", "SARC", "LIHC", "COAD", "KIRP", "OV", "KIRC",
    "SKCM", "STAD", "BLCA", "UCEC", "LGG", "HNSC", "LUSC", "LUAD", "BRCA")
symbols = c()
ids = c()
for (nom_cancer in cancers) {
    print(nom_cancer)
    con = file(paste0("../benchmark/data/", nom_cancer, ".csv"))
    header = strsplit(readLines(con, n=1), ",")[[1]]
    close(con)
    new_symbols = gsub("_mutation$", "",
        grep("_mutation$", header, value=TRUE))
    new_ids = unique(gsub("_cnv$|_rna$", "",
        grep("_cnv$|_rna$", header, value=TRUE)))
    symbols = c(symbols, setdiff(new_symbols, symbols))
    ids = c(ids, setdiff(new_ids, ids))
}
symbols_df = data.frame(
    orig_symbol=symbols,
    symbol_for_match=gsub("\\.|-", "?", symbols) #subsetting misused characters
)

# then we try with gencode22 gtf
url_gencode22 = "ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_22/gencode.v22.annotation.gff3.gz"
file_gencode22 = "gencode.v22.annotation.gff3.gz"

download.file(url_gencode22, destfile=file_gencode22)

g22 = read.delim(gzfile(file_gencode22), header=F, comment.char="#")
g22 = g22 |>
    subset(V3=="gene") |>
    _[,"V9"] |>
    strsplit(";") |>
    lapply("[", 1:5) |>
    lapply(\(x) sub("^.*=", "", x)) |>
    do.call(rbind.data.frame, args=_)
#colnames(gene_table) = sub("=.*$", "", gene_table[1,])
colnames(g22) = c("ID", "gene_id", "gene_type", "gene_status", "gene_name")

g22_save = g22
g22 = g22 |>
    transform(ensembl_gene_id=sub("\\..*$", "", gene_id)) |> # ENSGXXXXXX.YY, remove YY
    transform(hgnc_symbol=gene_name) |> 
    transform(symbol_for_match = gsub("\\.|-", "?", gene_name)) |>
    subset(select=c(ensembl_gene_id, hgnc_symbol, symbol_for_match))

gene_map = merge(symbols_df, g22, all.x=TRUE)

dup_genes = gene_map |>
    _[["orig_symbol"]] |>
    table() |>
    data.frame() |>
    subset(Freq>1) |>
    _[["Var1"]] |>
    as.character()
gene_map |>
    subset(orig_symbol %in% dup_genes, select=c(hgnc_symbol, ensembl_gene_id))

gene_map |>
    subset(is.na(ensembl_gene_id)) |>
    nrow() # 95 genes still lost

lost_genes = gene_map |>
    subset(is.na(ensembl_gene_id)) |>
    _[["orig_symbol"]]
intersect(lost_genes, dup_genes)

# let's try querying ensembl
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl", version="80")

gene_map = gene_map |>
    subset(is.na(ensembl_gene_id)) |>
    _[["orig_symbol"]] |>
    getBM(
        attributes=c("ensembl_gene_id", "hgnc_symbol"),
        filters="hgnc_symbol",
        values=_,
        mart=ensembl
    ) |>
    transform(orig_symbol=hgnc_symbol) |>
    merge(x=gene_map, y=_, all.x=TRUE,
        by.x="orig_symbol", by.y="orig_symbol", suffixes=c("", ".new")) |>
    transform(ensembl_gene_id=ifelse(
        is.na(ensembl_gene_id),
        ensembl_gene_id.new,
        ensembl_gene_id)) |>
    transform(hgnc_symbol=ifelse(
        is.na(hgnc_symbol),
        hgnc_symbol.new,
        hgnc_symbol)) |>
    subset(select=-c(ensembl_gene_id.new, hgnc_symbol.new))

dup_genes = gene_map |>
    _[["orig_symbol"]] |>
    table() |>
    data.frame() |>
    subset(Freq>1) |>
    _[["Var1"]] |>
    as.character()
gene_map |>
    subset(orig_symbol %in% dup_genes, select=c(hgnc_symbol, ensembl_gene_id))

gene_map |>
    subset(is.na(ensembl_gene_id)) |>
    nrow() # 10 genes still lost

lost_genes = gene_map |>
    subset(is.na(ensembl_gene_id)) |>
    _[c("orig_symbol", "symbol_for_match")] |>
    unique()
intersect(lost_genes$orig_symbol, dup_genes)

subset(gene_map, is.na(ensembl_gene_id))

# finally let's try reverse engineering the original datasets ids
gene_map_rev = ids |>
    getBM(
        attributes=c("ensembl_gene_id", "hgnc_symbol"),
        filters="ensembl_gene_id",
        values=_,
        mart=ensembl
    ) |>
    subset(hgnc_symbol!="") |>
    transform(symbol_for_match = gsub("\\.|-", "?", hgnc_symbol)) |>
    subset(symbol_for_match %in% lost_genes$symbol_for_match) |> # checking only missing genes
    merge(x=gene_map, y=_, all.x=TRUE,
        by.x="symbol_for_match", by.y="symbol_for_match", suffixes=c("", ".new")) |>
    transform(ensembl_gene_id=ifelse(
        is.na(ensembl_gene_id),
        ensembl_gene_id.new,
        ensembl_gene_id)) |>
    transform(hgnc_symbol=ifelse(
        is.na(hgnc_symbol),
        hgnc_symbol.new,
        hgnc_symbol)) |>
    subset(select=-c(hgnc_symbol.new, ensembl_gene_id.new))

gene_map_rev |>
    subset(hgnc_symbol!=hgnc_symbol.new)

gene_map_rev |>
    subset(ensembl_gene_id!=ensembl_gene_id.new,
           select=c(orig_symbol, ensembl_gene_id, ensembl_gene_id.new, hgnc_symbol))

# got 3 more genes
gene_map_rev |>
    subset(is.na(ensembl_gene_id)) |>
    nrow() 
subset(gene_map_rev, is.na(ensembl_gene_id), select=c(orig_symbol))

# manual correction of some names
# can only get MIR6511B1, added it to the manual df below
corrected_hgnc = data.frame(
    hgnc_symbol=c(
        "C6orf49",
        "CSNK2B-LY6G5B--991",
        "MIR1235",
        "MIR6511B1",
        "RP11-146B14.1",
        "RP13-1032I1.10"
    ),
    orig_symbol=c(
        "C6orf49",
        "CSNK2B.LY6G5B..991",
        "hsa.mir.1253",
        "hsa.mir.6511b.1",
        "RP11.146B14.1",
        "RP13.1032I1.10"
    )
)
corrected_hgnc = corrected_hgnc |>
    _[["hgnc_symbol"]] |>
    getBM(
        attributes=c("ensembl_gene_id", "hgnc_symbol"),
        filters="hgnc_symbol",
        values=_,
        mart=ensembl
    ) |>
    merge(x=corrected_hgnc, y=_, all.x=TRUE)

manual = data.frame(
    hgnc_symbol=c(
        "C6orf49",
        "CSNK2B-LY6G5B--991",
        "MIR1253",
        "hsa-mir-1253",
        "MIR6511B1",
        "RP11-146B14.1",
        "RP13-1032I1.10"
    ),
    orig_symbol=c(
        "C6orf49",
        "CSNK2B.LY6G5B..991",
        "hsa.mir.1253",
        "hsa.mir.1253",
        "hsa.mir.6511b.1",
        "RP11.146B14.1",
        "RP13.1032I1.10"
    ),
    ensembl_gene_id=c(
        "ENSG00000278224",
        "ENSG00000263020",
        "ENSG00000221200", #MIR1253 in ensembl
        "ENSG00000272920", #hsa-mir-1253 in ensembl, annotation pb?
        "ENSG00000273879",
        "ENSG00000274527",
        "ENSG00000262660"
    )
)

gene_map_rev = merge(x=gene_map_rev, y=manual, all.x=TRUE,
        by.x="orig_symbol", by.y="orig_symbol", suffixes=c("", ".new")) |>
    transform(ensembl_gene_id=ifelse(
        is.na(ensembl_gene_id),
        ensembl_gene_id.new,
        ensembl_gene_id)) |>
    transform(hgnc_symbol=ifelse(
        is.na(hgnc_symbol),
        hgnc_symbol.new,
        hgnc_symbol)) |>
    subset(select=-c(hgnc_symbol.new, ensembl_gene_id.new))

gene_map_rev |>
    subset(is.na(ensembl_gene_id)) # victoire
# only Unknown stay without ENSG

#write.csv(gene_map_rev, "vlegoff_gene_id_map.csv", col.names=FALSE)
readr::write_csv(gene_map_rev, "vlegoff_gene_id_map.csv")

# sanity/hope checking
getBM(
    attributes=c("ensembl_gene_id", "hgnc_symbol"),
    filters="ensembl_gene_id",
    values=checked_ensg_by_hand$ensembl_gene_id,
    mart=ensembl
)

manual = merge(corrected_hgnc, checked_ensg_by_hand, all.x=TRUE, all.y=TRUE,
    by=c("orig_symbol"), suffixes=c("", ".y")) |>
    transform(ensembl_gene_id=ifelse(is.na(ensembl_gene_id), ensembl_gene_id.y,
        ensembl_gene_id)) |>
    subset(select=-c(ensembl_gene_id.y))


# sanity checking ids
gene_map_rev |>
    subset(!is.na(ensembl_gene_id) & !is.na(ensembl_gene_id.new)) |>
    subset(ensembl_gene_id!=ensembl_gene_id.new,
        select=c(ensembl_gene_id, ensembl_gene_id.new, hgnc_symbol))
