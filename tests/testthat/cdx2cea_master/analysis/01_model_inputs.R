################################################################################ 
# This script generates all the required input parameters for the cohort STM   #
# used to evaluated the cost-effectiveness of testing for the absence of CDX2  #
# biomarker followed by adjuvant chemotherapy for stage II colon cancer        #
# patients.                                                                    #
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

#### 01.1 Load packages and functions ####
#### 01.1.1 Load packages and functions ####
library(cdx2cea)

#### 01.1.2 Load functions ####
# no required functions

#### 01.2 Load all parameters ####
l_params_all <- load_all_params()
l_params_all
