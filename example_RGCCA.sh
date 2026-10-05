micromamba activate -p env/inti_el9

# creating and populating batchtool registry with jobs to run
Rscript R/populate.R \
    -l learners/example2.conf.R \
    -r registries/example2 \
    -d data/TCGA_example2.conf.R \
    -N 8 \
    -s 1234

# running the jobs in parallel
Rscript R/run.R \
    -m multicore \
    -r registries/example2 \
    -N 8 \
    --resubmit_not_finished #\

# extracting results
Rscript R/results.R \
    -r registries/example2 \
    -d results/example2 \
    -m measures.conf.R \
    -N 8 \
    -C 100 -v

# getting best inner CV metric and lambda
Rscript R/cv_results.R \
    -f results/cv_logs/example2 \
    -c data/folds.rds \
    -d results/cv_logs/example2 \
    -N 8

tar -czvf results/cv_logs/example2/cv_logs.tar.gz results/cv_logs/example/*.rds \
    --remove-files
