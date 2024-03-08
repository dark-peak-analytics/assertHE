################################################################################ 
# This script runs the deterministic cost-effectiveness analysis of testing    #
# Stage II colon cancer patients for the absence of CDX2 biomarker followed by #
# adjuvant chemotherapy.                                                       #
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

re_run <- FALSE # TRUE

#### 05b.1 Load packages and functions ####
#### 05b.1.1 Load packages ####
library(cdx2cea)
# devtools::install_github("DARTH-git/dampack") # Uncomment if dampack not installed
library(dampack) 
library(dplyr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(patchwork)

#### 05b.1.2 Load inputs ####
l_params_all <- load_all_params() # function in cdx2cea

#### 05b.1.3 Load functions ####
# no required functions

#### 05b.1.4 Load base-case parameters ####
### Load MAP as best calibration parameter set for deterministic analysis
data("v_calib_post_map")

### Parameters for base-case
## Update base-case parameters with calibrated values at MAP
l_params_basecase <- update_param_list(l_params_all, v_calib_post_map)

#### 05b.2 Time spent in each state ####
#### 05b.2.1 CDX2-negative patients ####
### Run model with treatment
l_cdx2neg_trt <- ce_model(l_params_all = l_params_basecase, 
                          p_CDX2neg_init = 1, Trt = TRUE)
### Run model without treatment
l_cdx2neg_notrt <- ce_model(l_params_all = l_params_basecase, 
                            p_CDX2neg_init = 1, Trt = FALSE)
## Extract life years
ly_cdx2neg_trt   <- l_cdx2neg_trt$tot_ly_und
ly_cdx2neg_notrt <- l_cdx2neg_notrt$tot_ly_und
## Extract cohort trace
m_M_cdx2neg_trt   <- l_cdx2neg_trt$m_M
m_M_cdx2neg_notrt <- l_cdx2neg_notrt$m_M

### Life years gained under treatment 
ly_gain_cdx2neg      <- (ly_cdx2neg_trt - ly_cdx2neg_notrt)  ### RESULT!
ly_gain_cdx2neg_prop <- (ly_cdx2neg_trt - ly_cdx2neg_notrt)/ly_cdx2neg_notrt ### RESULT!

### Time spent without and with recurrence
df_ly_rec_cdx2neg <- data.frame(`Health State` = ordered(c("Without recurrence", "With recurrence"),
                                                         c("With recurrence", "Without recurrence")),
                            `No FOLFOX` = c(sum(m_M_cdx2neg_notrt[, c("CDX2pos", "CDX2neg")]), 
                                            sum(m_M_cdx2neg_notrt[, "Mets"]))/12,
                            `FOLFOX`    = c(sum(m_M_cdx2neg_trt[, c("CDX2pos", "CDX2neg")]), 
                                            sum(m_M_cdx2neg_trt[, "Mets"]))/12,
                            check.names = FALSE
)
df_ly_rec_cdx2neg_lng <- reshape2::melt(df_ly_rec_cdx2neg, 
                                        id.vars = "Health State", 
                                        variable.name = "Strategy", 
                                        value.name = "Years")
df_charts_ly_rec_cdx2neg <- df_ly_rec_cdx2neg_lng %>% 
  dplyr::group_by(.data$Strategy) %>%
  dplyr::mutate(pos = cumsum(.data$Years) - (0.5 * .data$Years))
df_charts_ly_rec_cdx2neg

#### 05b.2.2 CDX2-positive patients ####
### Run model with treatment
l_cdx2pos_trt <- ce_model(l_params_all = l_params_basecase, 
                          p_CDX2neg_init = 0, Trt = TRUE)
### Run model without treatment
l_cdx2pos_notrt <- ce_model(l_params_all = l_params_basecase, 
                            p_CDX2neg_init = 0, Trt = FALSE)
## Extract life years
ly_cdx2pos_trt   <- l_cdx2pos_trt$tot_ly_und
ly_cdx2pos_notrt <- l_cdx2pos_notrt$tot_ly_und
## Extract cohort trace
m_M_cdx2pos_trt   <- l_cdx2pos_trt$m_M
m_M_cdx2pos_notrt <- l_cdx2pos_notrt$m_M

### Life years gained under treatment 
ly_cdx2pos_trt ### RESULT!
ly_gain_cdx2pos <- (ly_cdx2pos_trt - ly_cdx2pos_notrt)/ly_cdx2pos_notrt ### RESULT!

### Time spent without and with recurrence
df_ly_rec_cdx2pos <- data.frame(`Health State` = ordered(c("Without recurrence", "With recurrence"),
                                                         c("With recurrence", "Without recurrence")),
                                `No FOLFOX` = c(sum(m_M_cdx2pos_notrt[, c("CDX2pos", "CDX2neg")]), 
                                                sum(m_M_cdx2pos_notrt[, "Mets"]))/12,
                                `FOLFOX`    = c(sum(m_M_cdx2pos_trt[, c("CDX2pos", "CDX2neg")]), 
                                                sum(m_M_cdx2pos_trt[, "Mets"]))/12,
                                check.names = FALSE
)
df_ly_rec_cdx2pos_lng <- reshape2::melt(df_ly_rec_cdx2pos, 
                                        id.vars = "Health State", 
                                        variable.name = "Strategy", 
                                        value.name = "Years")
df_charts_ly_rec_cdx2pos <- df_ly_rec_cdx2pos_lng %>% 
  dplyr::group_by(.data$Strategy) %>%
  dplyr::mutate(pos = cumsum(.data$Years) - (0.5 * .data$Years))
df_charts_ly_rec_cdx2pos

#### 05b.2.3 For both types of patients ####
v_p_CDX2_names <- c(paste0("CDX2-negative patients (", 
                           scales::percent(round(l_params_basecase$p_CDX2neg, 3), accuracy = 0.1), ")"),
                    paste0("CDX2-positive patients (", 
                           scales::percent(round(1-l_params_basecase$p_CDX2neg, 3), accuracy = 0.1), ")")) 
df_ly_rec_all <- rbind(
  cbind(Group = v_p_CDX2_names[1], 
        df_ly_rec_cdx2neg_lng),
  cbind(Group = v_p_CDX2_names[2], 
        df_ly_rec_cdx2pos_lng)) %>%
  filter(!(Group == v_p_CDX2_names[2] & Strategy == "FOLFOX"))
df_charts_ly_rec_all <- rbind(
  cbind(Group = v_p_CDX2_names[1], 
        data.frame(df_charts_ly_rec_cdx2neg, check.names = F)),
  cbind(Group = v_p_CDX2_names[2], 
        data.frame(df_charts_ly_rec_cdx2pos, check.names = F))) %>%
  filter(!(Group == v_p_CDX2_names[2] & Strategy == "FOLFOX"))

gg_ly <- ggplot(df_ly_rec_all, 
       aes(x = Strategy, y = Years, fill = `Health State`)) +
  facet_wrap(~ Group, ncol = 1, scales = "free_y") +
  geom_bar(stat="identity", position = "stack") +
  geom_text(data = df_charts_ly_rec_all, 
            aes(x = Strategy, y = pos, 
                label = format(round(Years, 2), nsmall = 2), group = `Health State`),
            size=5) +
  # scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  scale_fill_grey(start = 0.8, end = 0.5) +
  xlab("") + 
  scale_y_continuous("Life expectancy (years)", 
                     breaks = dampack::number_ticks(8)) +
  coord_flip() +
  guides(fill = guide_legend(title = "", reverse = T)) +
  theme_bw(base_size = 17) + 
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 14)) +
  NULL
gg_ly
ggsave(plot = gg_ly, filename = "figs/05b_time-states-all-patients.pdf", width = 10, height = 7)
ggsave(plot = gg_ly, filename = "figs/05b_time-states-all-patients.png", width = 10, height = 7, dpi = 300)
ggsave(plot = gg_ly, filename = "figs/manuscript/Figure 2 - time-states-all-patients.pdf", width = 10, height = 7, dpi = 300)
ggsave(plot = gg_ly, filename = "figs/manuscript/Figure 2 - time-states-all-patients.png", width = 10, height = 7, dpi = 300)
ggsave(plot = gg_ly, filename = "figs/manuscript/Figure 2 - time-states-all-patients.tiff", width = 10, height = 7, dpi = 300)

#### 05b.3 Cost-effectiveness analysis parameters ####
### Strategy names
v_names_str <- l_params_all$v_names_str
### Number of strategies
n_str <- length(v_names_str)

#### 05b.3 Compute cost-effectiveness outcomes ####
df_out_ce <- calculate_ce_out(l_params_all = l_params_basecase, 
                              n_wtp = 150000)
df_out_ce

#### 05b.4 Conduct CEA with deterministic output ####
### Calculate incremental cost-effectiveness ratios (ICERs)
df_cea_det <- dampack::calculate_icers(cost       = df_out_ce$Cost, 
                                       effect     = df_out_ce$Effect, 
                                       strategies = v_names_str)
df_cea_det$Strategy <- c("No CDX2 testing and no FOLFOX", 
                         "CDX2 testing and FOLFOX if CDX2-negative")
df_cea_det

### Save CEA table with ICERs
## As .RData
save(df_cea_det,
     file = "tables/05b_deterministic_cea_results.RData")
## As .csv
write.csv(df_cea_det, 
          file = "tables/05b_deterministic_cea_results.csv")

#### 05b.5 Plot cost-effectiveness frontier ####
plot(df_cea_det)
ggsave("figs/05b_cea_frontier.png", width = 8, height = 6)

#### 05b.6 One-way sensitivity analysis (OWSA) ####
l_bounds <- generate_params_bounds(l_params_all = l_params_basecase)
### Define OWSA design
df_owsa_input <- data.frame(pars = c("p_CDX2neg", "hr_Recurr_CDXneg_Rx",
                                     "c_Test", "u_Mets", "hr_RecurCDX2neg"),
                            min = c(l_bounds$v_lb$p_CDX2neg, 
                                    l_bounds$v_lb$hr_Recurr_CDXneg_Rx, 
                                    l_bounds$v_lb$c_Test,
                                    l_bounds$v_lb$u_Mets,
                                    1.00),
                            max = c(l_bounds$v_ub$p_CDX2neg, 
                                    0.975, 
                                    l_bounds$v_ub$c_Test,
                                    0.7,
                                    l_bounds$v_ub$hr_RecurCDX2neg))
### Run OWSA
if(re_run){
  df_owsa_icer <- run_owsa_det(params_range = df_owsa_input,
                              params_basecase = l_params_basecase,
                              FUN = calculate_ce_out, # Function to compute outputs
                              outcomes = "ICER",      # Output to do the OWSA on
                              strategies = v_names_str
                              )
  ### Rename parameters for plotting
  df_owsa_icer$parameter <- ordered(df_owsa_icer$parameter, 
                                    labels = c("Cost of test ($)",
                                               "Increased recurrence in CDX2-negative patients\nas a HR",
                                               "Effectiveness of FOLFOX in CDX2-negative\npatients as a HR",
                                               "Proportion of CDX2-negative patients",
                                               "Utility of metastatic recurrence"))
  
  df_owsa_icer %>% filter(parameter == "Effectiveness of FOLFOX in CDX2-negative\npatients as a HR", 
                          outcome_val <= 100000) %>%
    head(6)
  df_owsa_icer %>% filter(parameter == "Effectiveness of FOLFOX in CDX2-negative\npatients as a HR", 
                          outcome_val <= 100000) %>%
    tail(6)
  df_owsa_icer %>% filter(parameter == "Proportion of CDX2-negative patients", 
                          param_val %in% c(0.015, 0.15))
  df_owsa_icer %>% filter(parameter == "Increased recurrence in CDX2-negative patients\nas a HR", 
                          param_val == 1.69 | param_val > 4.320)
  df_owsa_icer %>% filter(parameter == "Increased recurrence in CDX2-negative patients\nas a HR")
  df_owsa_icer %>% filter(parameter == "Utility of metastatic recurrence", 
                          param_val %in% c(0.20, 0.70))
  save(df_owsa_icer,
       file = "data/05b_owsa_icer.RData")
}

### Plot OWSA
load(file = "data/05b_owsa_icer.RData")
gg_owsa <- plot(df_owsa_icer, 
                txtsize = 16, 
                n_x_ticks = 5, n_y_ticks = 5,
                facet_ncol = 2,
                facet_scales = "free") +
  scale_y_continuous("$/QALY",
                     breaks = equal_breaks(n=5, s=0.05), 
                     label = function(x){scales::comma(x)}, 
                     expand = c(0.05, 0)) + 
  theme(legend.position = "",
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 12))
gg_owsa
### Save OWSA figure
ggsave(plot = gg_owsa, 
       filename = "figs/05b_owsa_icer.png", 
       width = 10, height = 8)
ggsave(plot = gg_owsa, 
       filename = "figs/05b_owsa_icer.pdf", 
       width = 10, height = 8)
ggsave(plot = gg_owsa, 
       filename = "figs/manuscript/Figure 3 - OWSA.png", 
       width = 10, height = 8, dpi = 300)
ggsave(plot = gg_owsa, 
       filename = "figs/manuscript/Figure 3 - OWSA.pdf", 
       width = 10, height = 8, dpi = 300)
ggsave(plot = gg_owsa, 
       filename = "figs/manuscript/Figure 3 - OWSA.tiff", 
       width = 10, height = 8, dpi = 300)

#### 05b.7 Two-way sensitivity analysis (TWSA) on NMB ####

#### 05b.7.1 Proportion of CDX2-negative vs Effectiveness of FOLFOX in CDX2-negative patients (HR) ####
### Define TWSA designs
df_twsa_input_pCDX2_vs_hrCDX2negtrt <- data.frame(pars = c("p_CDX2neg", 
                                                           "hr_Recurr_CDXneg_Rx"),
                            min = c(0.015, 0.630),
                            max = c(0.150, 1.000))

### $50K/QALY
## Run TWSA
if(re_run){
  df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k <- run_twsa_det(params_range = df_twsa_input_pCDX2_vs_hrCDX2negtrt,
                              params_basecase = l_params_basecase,
                              FUN = calculate_ce_out, # Function to compute outputs
                              # nsamp = 10, 
                              outcomes = "NMB",      # Output to do the OWSA on
                              strategies = v_names_str, # Names of strategies
                              n_wtp = 50000        # Extra argument to pass to FUN
  )
  ### Rename strategies for plotting
  df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k$strategy <- factor(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k$strategy, 
                                                            levels = unique(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k$strategy),
                                                            c("(1) No CDX2 testing and no FOLFOX",
                                                              "(2) CDX2 testing and FOLFOX if CDX2-negative"))
  ### Rename parameters for plotting
  colnames(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k)[1:2] <- c("Proportion of CDX2-negative patients",
                                                             "Effectiveness of FOLFOX in CDX2-negative patients (HR)")
  save(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k,
       file = "data/05b_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k.RData")
}
### Load TWSA
load(file = "data/05b_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k.RData")
opt_nmb_pCDX2_vs_hrCDX2negtrt_50k <- df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k %>%
  group_by(.data[["Proportion of CDX2-negative patients"]],
           .data[["Effectiveness of FOLFOX in CDX2-negative patients (HR)"]]) %>%
  slice(which.max(.data$outcome_val))
table(opt_nmb_pCDX2_vs_hrCDX2negtrt_50k$strategy)/nrow(opt_nmb_pCDX2_vs_hrCDX2negtrt_50k)

### Plot TWSA
gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k <- plot(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k,
                                               col = c("bw")) +
  theme(legend.position = "bottom")
gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k 

### Save TWSA figures
ggsave(plot = gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k,
       filename = "figs/05b_twsa_p_CDX2neg_hr_RecurrCDXnegRx_nmb_50k.png", 
       width = 8, height = 6)
ggsave(plot = gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k,
       filename = "figs/05b_twsa_p_CDX2neg_hr_RecurrCDXnegRx_nmb_50k.pdf", 
       width = 8, height = 6)

### $100K/QALY
## Run TWSA
if(re_run){
  df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k <- run_twsa_det(params_range = df_twsa_input_pCDX2_vs_hrCDX2negtrt,
                                                         params_basecase = l_params_basecase,
                                                         FUN = calculate_ce_out, # Function to compute outputs
                                                         outcomes = "NMB",      # Output to do the OWSA on
                                                         strategies = v_names_str, # Names of strategies
                                                         n_wtp = 100000        # Extra argument to pass to FUN
  )
  ### Rename strategies for plotting
  df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k$strategy <- factor(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k$strategy, 
                                                            levels = unique(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k$strategy),
                                                            c("(1) No CDX2 testing and no FOLFOX",
                                                              "(2) CDX2 testing and FOLFOX if CDX2-negative"))
  ### Rename parameters for plotting
  colnames(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k)[1:2] <- c("Proportion of CDX2-negative patients",
                                                              "Effectiveness of FOLFOX in CDX2-negative patients (HR)")
  save(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k,
       file = "data/05b_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k.RData")
}
### Load TWSA
load(file = "data/05b_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k.RData")
opt_nmb_pCDX2_vs_hrCDX2negtrt_100k <- df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k %>%
  group_by(.data[["Proportion of CDX2-negative patients"]],
           .data[["Effectiveness of FOLFOX in CDX2-negative patients (HR)"]]) %>%
  slice(which.max(.data$outcome_val))
table(opt_nmb_pCDX2_vs_hrCDX2negtrt_100k$strategy)/nrow(opt_nmb_pCDX2_vs_hrCDX2negtrt_100k)

### Plot TWSA
gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k <- plot(df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k,
                                                col = c("bw")) +
  theme(legend.position = "bottom")
gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k 
### Save TWSA figures
ggsave(plot = gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k,
       filename = "figs/05b_twsa_p_CDX2neg_hr_RecurrCDXnegRx_nmb_100k.png", 
       width = 8, height = 6)
ggsave(plot = gg_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k,
       filename = "figs/05b_twsa_p_CDX2neg_hr_RecurrCDXnegRx_nmb_100k.pdf", 
       width = 8, height = 6)

#### 05b.7.2 Increased recurrence in CDX2-negative patients vs Effectiveness of FOLFOX in CDX2-negative patients (HR) ####
### Define TWSA designs
df_twsa_input_hrRecurCDX2neg_vs_hrCDX2negtrt <- data.frame(pars = c("hr_RecurCDX2neg",
                                                                    "hr_Recurr_CDXneg_Rx"),
                                                  min = c(1.00, 0.630),
                                                  max = c(4.38, 1.000))

### $50K/QALY
## Run TWSA
if(re_run){
  df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k <- run_twsa_det(params_range = df_twsa_input_hrRecurCDX2neg_vs_hrCDX2negtrt,
                                                         params_basecase = l_params_basecase,
                                                         FUN = calculate_ce_out, # Function to compute outputs
                                                         outcomes = "NMB",      # Output to do the OWSA on
                                                         strategies = v_names_str, # Names of strategies
                                                         n_wtp = 50000        # Extra argument to pass to FUN
  )
  ### Rename strategies for plotting
  df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k$strategy <- factor(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k$strategy, 
                                                                     levels = unique(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k$strategy),
                                                                     c("(1) No CDX2 testing and no FOLFOX",
                                                                       "(2) CDX2 testing and FOLFOX if CDX2-negative"))
  ### Rename parameters for plotting
  colnames(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k)[1:2] <- c("Increased recurrence in CDX2-negative patients as a HR",
                                                                     "Effectiveness of FOLFOX in CDX2-negative patients (HR)")
  save(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k,
       file = "data/05b_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k.RData")
}
### Load TWSA
load(file = "data/05b_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k.RData")
opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k <- df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k %>%
  group_by(.data[["Increased recurrence in CDX2-negative patients as a HR"]],
           .data[["Effectiveness of FOLFOX in CDX2-negative patients (HR)"]]) %>%
  slice(which.max(.data$outcome_val))
table(opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k$strategy)/nrow(opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k)
opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k %>% 
  filter(`Increased recurrence in CDX2-negative patients as a HR` == 1.00) %>%
  View()

### Plot TWSA
gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k <- plot(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k,
                                                        col = c("bw")) +
  theme(legend.position = "bottom")
gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k 
### Save TWSA figures
ggsave(plot = gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k,
       filename = "figs/05b_twsa_hr_Recurr_CDXneg_Rx_RecurrCDXnegRx_nmb_50k.png", 
       width = 8, height = 6)
ggsave(plot = gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k,
       filename = "figs/05b_twsa_hr_Recurr_CDXneg_Rx_RecurrCDXnegRx_nmb_50k.pdf", 
       width = 8, height = 6)

### $100K/QALY
## Run TWSA
if(re_run){
  df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k <- run_twsa_det(params_range = df_twsa_input_hrRecurCDX2neg_vs_hrCDX2negtrt,
                                                                  params_basecase = l_params_basecase,
                                                                  FUN = calculate_ce_out, # Function to compute outputs
                                                                  outcomes = "NMB",      # Output to do the OWSA on
                                                                  strategies = v_names_str, # Names of strategies
                                                                  n_wtp = 100000        # Extra argument to pass to FUN
  )
  ### Rename strategies for plotting
  df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k$strategy <- factor(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k$strategy, 
                                                                     levels = unique(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k$strategy),
                                                                     c("(1) No CDX2 testing and no FOLFOX",
                                                                       "(2) CDX2 testing and FOLFOX if CDX2-negative"))
  ### Rename parameters for plotting
  colnames(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k)[1:2] <- c("Increased recurrence in CDX2-negative patients as a HR",
                                                                      "Effectiveness of FOLFOX in CDX2-negative patients (HR)")
  save(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k,
       file = "data/05b_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k.RData")
}
### Load TWSA
load(file = "data/05b_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k.RData")
opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k <- df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k %>%
  group_by(.data[["Increased recurrence in CDX2-negative patients as a HR"]],
           .data[["Effectiveness of FOLFOX in CDX2-negative patients (HR)"]]) %>%
  slice(which.max(.data$outcome_val))
table(opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k$strategy)/nrow(opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k)
opt_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k %>% 
  filter(`Increased recurrence in CDX2-negative patients as a HR` == 1.00) %>%
  View()


### Plot TWSA
gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k <- plot(df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k,
                                                         col = c("bw")) +
  theme(legend.position = "bottom")
gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k 
### Save TWSA figure
ggsave(plot = gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k,
       filename = "figs/05b_twsa_hr_Recurr_CDXneg_Rx_RecurrCDXnegRx_nmb_100k.png", 
       width = 8, height = 6)
ggsave(plot = gg_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k,
       filename = "figs/05b_twsa_hr_Recurr_CDXneg_Rx_RecurrCDXnegRx_nmb_100k.pdf", 
       width = 8, height = 6)

#### 05b.7.3 Combine both TWSAs ####
### Proportion of CDX2-negative vs Effectiveness of FOLFOX in CDX2-negative patients (HR)
## Create data.frame with base-case values for parameters in SA
df_basecase_pCDX2_vs_hrCDX2negtrt <- data.frame(x = l_params_basecase$p_CDX2neg,
                                                y = l_params_basecase$hr_Recurr_CDXneg_Rx)

df_twsa_pCDX2_vs_hrCDX2negtrt <- dplyr::bind_rows(data.frame(WTP = "$50,000/QALY", 
                                                      df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_50k, 
                                                      check.names = FALSE),
                                           data.frame(WTP = "$100,000/QALY", 
                                                      df_twsa_nmb_pCDX2_vs_hrCDX2negtrt_100k,
                                                      check.names = FALSE))
df_twsa_pCDX2_vs_hrCDX2negtrt$WTP <- ordered(df_twsa_pCDX2_vs_hrCDX2negtrt$WTP, 
                                             c("$50,000/QALY", "$100,000/QALY"))
# parameter names
params <- names(df_twsa_pCDX2_vs_hrCDX2negtrt)[c(2, 3)]
param1 <- params[1]
param2 <- params[2]
obj_fn <- which.max
opt_df <- df_twsa_pCDX2_vs_hrCDX2negtrt %>%
  group_by(WTP, .data[[param1]], .data[[param2]]) %>%
  slice(obj_fn(.data$outcome_val))

gg_twsa_pCDX2_vs_hrCDX2negtrt <- ggplot(opt_df, aes_(x = as.name(param1), y = as.name(param2))) +
  geom_tile(aes_(fill = as.name("strategy"))) +
  facet_wrap(~ WTP) + 
  theme_bw()+ 
  xlab(param1) +
  ylab(param2) +
  theme(strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 14))
gg_twsa_pCDX2_vs_hrCDX2negtrt <- add_common_aes(gg_twsa_pCDX2_vs_hrCDX2negtrt, 16, col = "bw", 
               col_aes = "fill",
               scale_name = "Strategy",
               continuous = c("x", "y"),
               n_x_ticks = 6,
               n_y_ticks = 6,
               xexpand = c(0, 0),
               yexpand = c(0, 0)) +
  theme(legend.position = "",
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 14)) +
  annotate("text", label = "*", 
           x = df_basecase_pCDX2_vs_hrCDX2negtrt$x, 
           y = df_basecase_pCDX2_vs_hrCDX2negtrt$y, 
           size = 14, colour = "yellow")
gg_twsa_pCDX2_vs_hrCDX2negtrt

gg_twsa_pCDX2_vs_hrCDX2negtrt_alt <- ggplot(opt_df, aes_(x = as.name(param1), 
                            y = as.name(param2), 
                            alpha = as.name("WTP")),) +
  geom_tile(aes_(fill = as.name("strategy"))) +
  scale_alpha_discrete(range = c(0.9, 0.35)) + 
  theme_bw()+ 
  xlab(param1) +
  ylab(param2) +
  theme(strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 14))
gg_twsa_pCDX2_vs_hrCDX2negtrt_alt <- add_common_aes(gg_twsa_pCDX2_vs_hrCDX2negtrt_alt, 16, col = "bw", 
                       col_aes = "fill",
                       scale_name = "Strategy",
                       continuous = c("x", "y"),
                       n_x_ticks = 6,
                       n_y_ticks = 6,
                       xexpand = c(0, 0),
                       yexpand = c(0, 0)) +
  theme(#legend.position = "",
    strip.background = element_rect(fill = "white",
                                    color = "white"),
    strip.text = element_text(hjust = 0, face = "bold", 
                              size = 14))
gg_twsa_pCDX2_vs_hrCDX2negtrt_alt

### Increased recurrence in CDX2-negative patients vs Effectiveness of FOLFOX in CDX2-negative patients (HR)
### Create data.frame with base-case values for parameters in SA
df_basecase_hr_RecurCDX2neg_vs_hrCDX2negtrt <- data.frame(x = l_params_basecase$hr_RecurCDX2neg,
                                                          y = l_params_basecase$hr_Recurr_CDXneg_Rx)

df_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt <- dplyr::bind_rows(data.frame(WTP = "$50,000/QALY", 
                                                      df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_50k, 
                                                      check.names = FALSE),
                                           data.frame(WTP = "$100,000/QALY", 
                                                      df_twsa_nmb_hrRecurCDX2neg_vs_hrCDX2negtrt_100k,
                                                      check.names = FALSE))
df_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt$WTP <- ordered(df_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt$WTP, 
                                             c("$50,000/QALY", "$100,000/QALY"))
# parameter names
params <- names(df_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt)[c(2, 3)]
param1 <- params[1]
param2 <- params[2]
obj_fn <- which.max
opt_df <- df_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt %>%
  group_by(WTP, .data[[param1]], .data[[param2]]) %>%
  slice(obj_fn(.data$outcome_val))

gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt <- ggplot(opt_df, aes_(x = as.name(param1), y = as.name(param2))) +
  geom_tile(aes_(fill = as.name("strategy"))) +
  facet_wrap(~ WTP) + 
  theme_bw()+ 
  xlab(param1) +
  ylab(param2) +
  theme(strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 14))
gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt <- add_common_aes(gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt, 16, col = "bw", 
                                                col_aes = "fill",
                                                scale_name = "Strategy",
                                                continuous = c("x", "y"),
                                                n_x_ticks = 6,
                                                n_y_ticks = 6,
                                                xexpand = c(0, 0),
                                                yexpand = c(0, 0)) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 14)) +
  annotate("text", label = "*", 
           x = df_basecase_hr_RecurCDX2neg_vs_hrCDX2negtrt$x, 
           y = df_basecase_hr_RecurCDX2neg_vs_hrCDX2negtrt$y, 
           size = 14, colour = "yellow")
gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt

gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt_alt <- ggplot(opt_df, aes_(x = as.name(param1), 
                                                         y = as.name(param2), 
                                                         alpha = as.name("WTP")),) +
  geom_tile(aes_(fill = as.name("strategy"))) +
  scale_alpha_discrete(range = c(0.9, 0.35)) + 
  theme_bw()+ 
  xlab(param1) +
  ylab(param2) +
  theme(strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0, face = "bold", 
                                  size = 14))
gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt_alt <- add_common_aes(gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt_alt, 16, col = "bw", 
                                                    col_aes = "fill",
                                                    scale_name = "Strategy",
                                                    continuous = c("x", "y"),
                                                    n_x_ticks = 6,
                                                    n_y_ticks = 6,
                                                    xexpand = c(0, 0),
                                                    yexpand = c(0, 0)) +
  theme(#legend.position = "",
    strip.background = element_rect(fill = "white",
                                    color = "white"),
    strip.text = element_text(hjust = 0, face = "bold", 
                              size = 14))
gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt_alt

### COmbine all TWSA into one ggplot
patched <- gg_twsa_pCDX2_vs_hrCDX2negtrt/gg_twsa_hrRecurCDX2neg_vs_hrCDX2negtrt
gg_twsa <- patched + plot_annotation(tag_levels = 'A')
gg_twsa
ggsave(plot = gg_twsa,
       filename = "figs/Figure 4 - TWSA.png", 
       width = 8, height = 6)
ggsave(plot = gg_twsa,
       filename = "figs/manuscript/Figure 4 - TWSA.pdf", 
       width = 12, height = 14, dpi = 300)
ggsave(plot = gg_twsa,
       filename = "figs/manuscript/Figure 4 - TWSA.png", 
       width = 12, height = 14, dpi = 300)
ggsave(plot = gg_twsa,
       filename = "figs/manuscript/Figure 4 - TWSA.tiff", 
       width = 12, height = 14, dpi = 300)
