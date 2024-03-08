################################################################################ 
# This script conducts the value of information (VOI) analysis of the          #
# cost-effectiveness analysis (CEA) of testing Stage II colon cancer patients  #
# for the absence of CDX2 biomarker followed by adjuvant chemotherapy          #
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

#### 05c.1 Load packages, functions and data ####
#### 05c.1.1 Load packages ####
library(dampack)   # decision-analytic modeling visualization tool

#### 05c.1.2 Load functions ####
#### 05c.1.3 Load PSA dataset ####
data("l_psa")

#### 05c.2 Define VOI inputs ####
### Vector with willingness-to-pay (WTP) thresholds
v_wtp <- seq(0, 150000, by = 5000)

#### 05c.3 Expected value of perfect information (EVPI) ####
### Individual level
evpi_ind <- calc_evpi(wtp = v_wtp, psa = l_psa)
plot(evpi_ind, effect_units = "QALY", txtsize = 16, n_x_ticks = 8) +
  xlab("Cost-effectiveness threshold (Thousand $/QALY)") +
ggsave("figs/05c_evpi_ind.png", width = 8, height = 6)

### Population level
## Population affected by the decision
# Source: Siegel RL, Miller KD, Jemal A: Cancer statistics, 2019. CA Cancer J Clin 69:7–34, 2019
pop_evi <- sum(101000*0.33*(1/(1+0.03)^(1:10))) # Annual cases * proportion that are Stage II * ten years
## EVPI
evpi_pop <- calc_evpi(wtp = v_wtp, psa = l_psa, pop = pop_evi)
gg_evpi_pop <- plot(evpi_pop, effect_units = "QALY", txtsize = 16, n_x_ticks = 8) + 
  scale_y_continuous(name = "EVPI (Million $)",
                     breaks = number_ticks(9), 
                     labels = function(x){formatC((x)/1000000, digits = 0, format = "f")}) +
  xlab("Cost-effectiveness threshold (Thousand $/QALY)")
gg_evpi_pop
ggsave(plot = gg_evpi_pop, "figs/05c_evpi_pop.png", width = 8, height = 6)
ggsave(plot = gg_evpi_pop, "figs/05c_evpi_pop.pdf", width = 8, height = 6)
ggsave(plot = gg_evpi_pop, "figs/manuscript/fig05b_evpi_pop.png", width = 8, height = 6)
ggsave(plot = gg_evpi_pop, "figs/manuscript/fig05b_evpi_pop.pdf", width = 8, height = 6)
ggsave(plot = gg_evpi_pop, "figs/manuscript/fig05b_evpi_pop.tiff", width = 8, height = 6)

evpi_pop %>% filter(WTP %in% c(50000, 100000, 150000))

patched <- gg_ceac_ceaf/gg_evpi_pop
gg_ceac_ceaf_evpi_pop <- patched + plot_annotation(tag_levels = 'A')
gg_ceac_ceaf_evpi_pop

#### 05c.4 Expected value of partial perfect information (EVPPI) ####

#### 05c.5 Expected value of sample information (EVSI) ####
