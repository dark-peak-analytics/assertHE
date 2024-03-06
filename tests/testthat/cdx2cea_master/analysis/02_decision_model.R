################################################################################ 
# This script runs cohort STM used to evaluated the cost-effectiveness of      #
# testing for the absence of CDX2 biomarker followed by adjuvant chemotherapy  #
# for stage II colon cancer patients.                                          #
#                                                                              # 
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <falarid@stanford.edu>                  #
#     - Deb Schrag, MD, MPH                                                    #
#     - Karen M. Kuntz, ScD                                                    #
################################################################################ 
# The structure of this code follows DARTH's coding framework                  #
# https://github.com/DARTH-git/darthpack                                       #
################################################################################ 

rm(list = ls()) # to clean the workspace

#### 02.1 Load packages and functions ####
#### 02.1.1 Load packages and functions ####
library(cdx2cea)
library(ggplot2) # For visualization
library(dplyr)    # For data manipulation
library(survival) # For plotting state-transition diagram

#### 02.1.2 Load inputs ####
l_params_all <- load_all_params() # function in cdx2cea

#### 02.1.3 Load functions ####
# no functions required

#### 02.2 Run STM ####
### Create list of model output
l_out_stm <- decision_model(l_params_all = l_params_all, 
                            err_stop = TRUE, verbose = TRUE)

### Plot Markov cohort trace
gg_trace <- plot_trace(l_params_all, m_M = l_out_stm$m_M)
gg_trace
ggsave(gg_trace,
       filename = "figs/02_trace_plot.pdf",
       width = 8, height = 6)
ggsave(gg_trace,
       filename = "figs/02_trace_plot.png",
       width = 8, height = 6)
ggsave(gg_trace,
       filename = "figs/02_trace_plot.jpg",
       width = 8, height = 6)

### Plot state-transition diagram
png("figs/02_model_diagram.png")
  connect <- (l_out_stm$a_P[,,1] > 0)
  survival::statefig(layout = c(2, 2, 1), connect = connect)
dev.off()
