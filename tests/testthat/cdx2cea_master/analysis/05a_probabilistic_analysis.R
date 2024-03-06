################################################################################ 
# This script conducts the probabilistic sensitivity analysis (ProbSA) of the  #
# cost-effectiveness of testing Stage II colon cancer patients for the absence #
# of CDX2 biomarker followed by adjuvant chemotherapy                          #
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

#### 05a.1 Load packages and functions ####
#### 05a.1.1 Load packages ####
library(cdx2cea)
# PSA functionality
library(dampack)    # decision-analytic modeling visualization tool
library(doParallel)
library(ggplot2)
library(patchwork)

#### 05a.1.2 Load inputs ####
l_params_all <- load_all_params() # function in cdx2cea

#### 05a.1.3 Load functions ####
# no required functions

#### 05a.2 Cost-effectiveness analysis parameters ####
### Strategy names
v_names_str <- l_params_all$v_names_str
### Number of strategies
n_str <- length(v_names_str)

#### 05a.3 Setup probabilistic analysis ####
### Number of simulations
n_sim <- 1000

### Generate PSA input dataset
df_psa_input <- generate_psa_params(l_params_all = l_params_all)

#### 05a.4 Conduct probabilistic sensitivity analysis ####
### Run decision model on each parameter set of PSA input dataset to produce
l_out_probsa <- run_probsa(df_psa_input = df_psa_input, 
                           n_str = n_str, 
                           parallel = TRUE)
### PSA outputs for cost and effects
df_c <- l_out_probsa$Costs
df_e <- l_out_probsa$Effects

### Create PSA object for dampack
l_psa <- dampack::make_psa_obj(cost = df_c, 
                               effectiveness = df_e, 
                               parameters = df_psa_input, 
                               strategies = v_names_str)
# Format object for plotting
l_psa$strategies              <- c("No CDX2 testing and no FOLFOX", "CDX2 testing and FOLFOX if CDX2-negative")
colnames(l_psa$effectiveness) <- c("No CDX2 testing and no FOLFOX", "CDX2 testing and FOLFOX if CDX2-negative")
colnames(l_psa$cost)          <- c("No CDX2 testing and no FOLFOX", "CDX2 testing and FOLFOX if CDX2-negative")

#### 05a.5 Save PSA objects ####
save(df_psa_input, df_c, df_e, v_names_str, n_str,
     l_psa,
     file = "output/05a_psa_dataset.RData")

#### 05a.6 Create probabilistic analysis graphs ####
data("l_psa") # stored as data object in 'cdx2cea'

### Vector with willingness-to-pay (WTP) thresholds
v_wtp <- seq(0, 150000, by = 5000)

#### 05a.6.1 Cost-effectiveness scatter plot ####
plot(l_psa) + 
  theme(legend.position = "bottom")
ggsave("figs/05a_cea_plane_scatter.png", width = 8, height = 6)

#### 05a.6.2 Conduct CEA with probabilistic output ####
### Compute expected costs and effects for each strategy from the PSA
df_out_ce_psa <- summary(l_psa)
### Calculate incremental cost-effectiveness ratios (ICERs)
df_cea_psa <- dampack::calculate_icers(cost = df_out_ce_psa$meanCost, 
                                       effect = df_out_ce_psa$meanEffect,
                                       strategies = df_out_ce_psa$Strategy)
df_cea_psa
### Save CEA table with ICERs
## As .RData
# save(df_cea_psa, 
#      file = "tables/05a_probabilistic_cea_results.RData")
save(df_cea_psa, 
     file = "data/05a_probabilistic_cea_results.RData")
## As .csv
write.csv(df_cea_psa, 
          file = "tables/05a_probabilistic_cea_results.csv")

#### 05a.6.3 Plot cost-effectiveness frontier ####
plot(df_cea_psa)
ggsave("figs/05a_cea_frontier_psa.png", width = 8, height = 6)

#### 05a.6.4 Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ####
ceac_obj <- ceac(wtp = v_wtp, psa = l_psa)
### Regions of highest probability of cost-effectiveness for each strategy
summary(ceac_obj)
### CEAC & CEAF plot
gg_ceac_ceaf <- plot(ceac_obj, txtsize = 16, col = "bw") +
  xlab("Cost-effectiveness threshold (Thousand $/QALY)") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom")
gg_ceac_ceaf
ggsave(plot = gg_ceac_ceaf, "figs/05a_ceac_ceaf.png", width = 8, height = 6)
ggsave(plot = gg_ceac_ceaf, "figs/manuscript/fig05a_ceac_ceaf.png", width = 8, height = 6, dpi = 300)
ggsave(plot = gg_ceac_ceaf, "figs/manuscript/fig05a_ceac_ceaf.pdf", width = 8, height = 6, dpi = 300)
ggsave(plot = gg_ceac_ceaf, "figs/manuscript/fig05a_ceac_ceaf.tiff", width = 8, height = 6, dpi = 300)

#### 05a.6.3 Expected Loss Curves (ELCs) ####
## Population affected by the decision
# Source: Siegel RL, Miller KD, Jemal A: Cancer statistics, 2019. CA Cancer J Clin 69:7–34, 2019
pop_evi <- sum(101000*0.33*(1/(1+0.03)^(1:10))) # Annual cases * proportion that are Stage II * ten years

elc_obj <- calc_exp_loss(wtp = v_wtp, psa = l_psa)
elc_obj
gg_elc <- plot(elc_obj, log_y = TRUE, #col = "bw",
               txtsize = 16, n_x_ticks = 8, n_y_ticks = 6) +
  xlab("Cost-effectiveness threshold (Thousand $/QALY)") +
  scale_y_continuous(name = "Expected Loss (Million $)",
                     breaks = c(0, 50,100, 200, 500, 1000, 2000, 5000), 
                     trans = "log", 
                     labels = function(x){formatC((x*pop_evi)/1000000, digits = 0, format = "f")}) + 
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2)) +
  scale_color_grey(name = waiver(), start = 0.2, end = 0.8,
                   aesthetics = "color", drop = FALSE) +
  theme(legend.position = "bottom")
gg_elc
ggsave(plot = gg_elc, "figs/05a_elc.png", width = 8, height = 6)
ggsave(plot = gg_elc, "figs/manuscript/fig05c_elc.png", width = 8, height = 6, dpi = 300)
ggsave(plot = gg_elc, "figs/manuscript/fig05c_elc.pdf", width = 8, height = 6, dpi = 300)
ggsave(plot = gg_elc, "figs/manuscript/fig05c_elc.tiff", width = 8, height = 6, dpi = 300)

#### 05a.6.4  Expected value of perfect information (EVPI) ####
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

### Combine CEAC, CEAF & EVPI
patched <- gg_ceac_ceaf/gg_evpi_pop/gg_elc
gg_ceac_ceaf_evpi_pop_elc <- patched + plot_annotation(tag_levels = 'A')
gg_ceac_ceaf_evpi_pop_elc
ggsave(plot = gg_ceac_ceaf_evpi_pop_elc,
       filename = "figs/manuscript/Figure 5 - CEAC_CEAF_EVPI_ELC.pdf", 
       width = 8, height = 13, dpi = 300)
ggsave(plot = gg_ceac_ceaf_evpi_pop_elc,
       filename = "figs/manuscript/Figure 5 - CEAC_CEAF_EVPI_ELC.png", 
       width = 8, height = 13, dpi = 300)
ggsave(plot = gg_ceac_ceaf_evpi_pop_elc,
       filename = "figs/manuscript/Figure 5 - CEAC_CEAF_EVPI_ELC.tiff", 
       width = 8, height = 13, dpi = 300)
 