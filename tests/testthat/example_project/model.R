library(assertHE)

# source local functions
v_files <- list.files("./tests/testthat/example_project/R", full.names = TRUE)
for(i in v_files)  source(i)

# use default parameters from package
params <- readRDS("./tests/testthat/example_project/data/dummy_sickSickerModel_params.rds")

# run the model with the example parameters
run_sickSicker_model(params_ = params)

