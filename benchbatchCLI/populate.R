suppressMessages({
    library(benchbatch)
    library(data.table)
    library(argparse)
    library(parallel)
})

parser = ArgumentParser()
parser$add_argument("-l", "--learners", nargs=1,
    help="config file containing the learners to run", required=TRUE)
parser$add_argument("-d", "--data", nargs=1,
    help="config file containing the description of the datasets on which
    to run the learners", required=TRUE)
parser$add_argument("-r", "--registry", nargs=1,
    help="path to the batchtools registry to use.
    Will be created if it does not exists", required=TRUE)
parser$add_argument("-k", "--keep_models", action="store_true", default=FALSE,
    help="store computed models in registry.
    WARNING: may take a lot of space on disk")
parser$add_argument("-N", "--Ncpus", default=1L, type="integer", choices=1:36,
    help="number of cpu to use [default %(default)s]")
parser$add_argument("-s", "--seed", default=1234,
    help="seed used to create resampling folds [default %(default)s]")
args = parser$parse_args()

set.seed(args$seed)
data.table::setDTthreads(args$Ncpus)

data = parse_data(args$data, ncpus=args$Ncpus)
learners = parse_learners(args$learners)

registry_dir = args$registry
populate(registry_dir, data$tasks, learners, data$cv,
    store_models=args$keep_models)
