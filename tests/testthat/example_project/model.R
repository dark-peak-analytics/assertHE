rm(list = ls())

# devtools::install_github("dark-peak-analytics/sicksickerPack")
library(sicksickerPack)
library(miceadds)
library(assertHE)

# source local functions
#v_files <- list.files("./inst/example_project", full.names = T)
#v_files <- v_files[-grep(x = v_files, pattern = "example_script.R")]
#for(i in v_files)  source(i)

# use default parameters from package
params <- sicksickerPack::dummy_sickSickerModel_params

# run the model with the example parameters
run_sickSicker_model(params_ = params)

