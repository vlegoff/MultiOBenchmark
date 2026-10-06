micromamba activate -p env/inti_el9

# creating and populating batchtool registry with jobs to run
Rscript R/populate.R \
    -l configs/learners/example_cox.conf.R \
    -r registries/example_cox \
    -d configs/data/TCGA_example.conf.R \
    -N 8 \
    -s 1234

# running the jobs in parallel
Rscript R/run.R \
    -m multicore \
    -r registries/example_cox \
    -N 8 \
    --resubmit_not_finished #\

# extracting results
Rscript R/results.R \
    -r registries/example_cox \
    -d results/example_cox \
    -m configs/measures.conf.R \
    -N 8 \
    -C 100 -v
