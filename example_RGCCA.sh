micromamba activate -p env/inti_el9

# creating and populating batchtool registry with jobs to run
Rscript R/populate.R \
    -l configs/learners/example_RGCCA.conf.R \
    -r registries/example_RGCCA \
    -d configs/data/TCGA_example.conf.R \
    -N 8 \
    -s 1234

# running the jobs in parallel
Rscript R/run.R \
    -m multicore \
    -r registries/example_RGCCA \
    -N 8 \
    --resubmit_not_finished #\

# extracting results
Rscript R/results.R \
    -r registries/example_RGCCA \
    -d results/example_RGCCA \
    -m configs/measures.conf.R \
    -N 8 \
    -C 100 -v

# getting best inner CV metric and lambda
Rscript R/cv_results.R \
    -f results/cv_logs/example_RGCCA \
    -c data/folds.rds \
    -d results/cv_logs/example_RGCCA \
    -N 8

tar -czvf results/cv_logs/example2/cv_logs.tar.gz results/cv_logs/example/*.rds \
    --remove-files
