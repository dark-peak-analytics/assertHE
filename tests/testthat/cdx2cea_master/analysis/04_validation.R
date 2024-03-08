################################################################################ 
# This script conducts an internal validation of the cohort STM of Stage II    #
# colon cancer patients stratified by CDX2 biomarker status to survival curves #
# from Dalerba et al. (2016)                                                   #
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

#### 04.1 Load packages and functions ####
#### 04.1.1 Load packages ####
library(cdx2cea)

#### 04.1.2 Load inputs ####
l_params_init_valid <- load_params_init(n_age_init = 75, 
                                        n_age_max = 80)
l_params_all_valid <- load_all_params(l_params_init = l_params_init_valid)


#### 04.1.3 Load functions ####
# no required functions

#### 04.1.4 Load targets and calibrated parameters ####
data("03_calibration_targets")
data("m_calib_post")
data("v_calib_post_map")

#### 04.2 Compute model-predicted outputs ####
#### 04.2.1 Compute model-predicted outputs for each sample of posterior distribution ####
### Number of posterior samples
n_samp <- nrow(m_calib_post)

### Define matrices to store model outputs
m_dfs_neg <- matrix(NA, nrow = n_samp, ncol = 61)
m_dfs_pos <- matrix(NA, nrow = n_samp, ncol = 61)
m_os_neg  <- matrix(NA, nrow = n_samp, ncol = 61)
m_os_pos  <- matrix(NA, nrow = n_samp, ncol = 61)
m_dss_neg <- matrix(NA, nrow = n_samp, ncol = 61)
m_dss_pos <- matrix(NA, nrow = n_samp, ncol = 61)

### Create data frames with model predicted outputs
df_dfs_neg <- data.frame(Outcome = "DFS", CDX2 = "CDX2-Negative", m_dfs_neg)
df_dfs_pos <- data.frame(Outcome = "DFS", CDX2 = "CDX2-Positive", m_dfs_pos)
df_os_neg  <- data.frame(Outcome = "OS",  CDX2 = "CDX2-Negative", m_os_neg)
df_os_pos  <- data.frame(Outcome = "OS",  CDX2 = "CDX2-Positive", m_os_pos)
df_dss_neg <- data.frame(Outcome = "DSS", CDX2 = "CDX2-Negative", m_dss_neg)
df_dss_pos <- data.frame(Outcome = "DSS", CDX2 = "CDX2-Positive", m_dss_pos)

### Evaluate model at each posterior sample and store results
for(i in 1:n_samp){ # i = 1
  l_out_post <- calibration_out(v_params_calib = m_calib_post[i, ], 
                                  l_params_all = l_params_all_valid)
  df_dfs_neg[i, -c(1, 2)] <- l_out_post$v_dfs_CDX2neg
  df_dfs_pos[i, -c(1, 2)] <- l_out_post$v_dfs_CDX2pos
  df_os_neg[i, -c(1, 2)]  <- l_out_post$v_os_CDX2neg
  df_os_pos[i, -c(1, 2)]  <- l_out_post$v_os_CDX2pos
  df_dss_neg[i, -c(1, 2)] <- l_out_post$v_dss_CDX2neg 
  df_dss_pos[i, -c(1, 2)] <- l_out_post$v_dss_CDX2pos 
  cat('\r', paste(round(i/n_samp * 100), "% done", sep = " ")) # display progress
}

### Combine all outputs
df_out_valid <- dplyr::bind_rows(df_dfs_neg,
                                 df_dfs_pos,
                                 df_os_pos ,
                                 df_os_neg ,
                                 df_dss_pos,
                                 df_dss_neg)
### Rename time variable
colnames(df_out_valid)[3:ncol(df_out_valid)] <- 0:60

### Transform data.frame to long format
df_out_valid_lng <- reshape2::melt(df_out_valid,
                                   id.vars = c("Outcome", "CDX2"))
### Compute posterior-predicted 95% CI
df_out_valid_sum <- data_summary(df_out_valid_lng, varname = "value",
                                 groupnames = c("Outcome", "CDX2", "variable"))
df_out_valid_sum$Time <- as.numeric(df_out_valid_sum$variable)

### Only 5-yr survival
df_out_valid_5yr_sum <- df_out_valid_sum %>% 
  dplyr::filter(Time == 60) %>%
  dplyr::mutate(Source = "Model",
                N = NaN) %>%
  dplyr::select(Source, Outcome, CDX2, Time, S = value, se = sd, lb, ub)
df_out_valid_5yr_sum$Time <- df_out_valid_5yr_sum$Time-1

### Combine model-predicted outputs with targets
df_model_n_targets <- dplyr::bind_rows(df_out_valid_5yr_sum,
                                       df_calibration_targets)

#### 04.4 Internal validation: Model-predicted outputs vs. targets ####
gg_valid <- ggplot(df_model_n_targets,
       aes(x = Outcome, y = S,
           ymin = lb, ymax = ub,
           fill = Source, 
           shape = Source)) +
  # geom_point(position = position_dodge()) +
  facet_wrap(~CDX2) +
  # geom_bar(position = position_dodge(), 
  #          stat = "identity", alpha = 0.4) +
  geom_errorbar(aes(color = Source), 
                position = position_dodge2(width = 0.5, padding = 0.7)) +
  scale_color_manual(values = c("Calibration target" = "black", "Model" = "red")) +
  scale_fill_manual(values = c("Calibration target" = "black", "Model" = "red")) +
  scale_shape_manual(values = c("Calibration target" = 1, "Model" = 8)) +
  xlab("") +
  ylab("5-year survival") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(size = 14, face = "bold"))
gg_valid
ggsave(gg_valid,
       filename = "figs/04_validation_posterior_vs_targets.pdf",
       width = 8, height = 6)
ggsave(gg_valid,
       filename = "figs/04_validation_posterior_vs_targets.png",
       width = 8, height = 6)
ggsave(gg_valid,
       filename = "figs/04_validation_posterior_vs_targets.jpg",
       width = 8, height = 6)