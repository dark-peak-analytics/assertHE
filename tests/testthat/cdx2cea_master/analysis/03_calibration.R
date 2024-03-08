################################################################################ 
# This script calibrates the cohort STM of Stage II colon cancer patients      #
# stratified by CDX2 biomarker status to survival curves from Dalerba et al.   #
# (2016) using a Bayesian approach with the Incremental Mixture Importance     #
# Sampling (IMIS) algorithm                                                    #
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

#### 03.1 Load packages, data and functions ####
#### 03.1.1 Load packages and functions ####
# Install IMIS from CRAN archive
# devtools::install_version("IMIS", version = "0.1", repos = "http://cran.us.r-project.org")
library(IMIS)
# Dependencies have been loaded with 'cdx2cea'
library(cdx2cea)
library(logitnorm)
library(ggplot2)
library(doParallel)
library(GGally)

#### 03.1.2 Load inputs ####
l_params_init_calib <- load_params_init(n_age_init = 75, 
                                        n_age_max = 80)
l_params_all_calib <- load_all_params(l_params_init = l_params_init_calib)

#### 03.1.3 Load functions ####
# no required functions

#### 03.1.4 Load calibration targets ####
data("03_calibration_targets")

#### 03.2 Visualize targets ####
gg_targets <- ggplot(df_calibration_targets,
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
        legend.title = element_blank())
gg_targets
ggsave(gg_targets,
       filename = "figs/03_calibration_targets.pdf",
       width = 8, height = 6)
ggsave(gg_targets,
       filename = "figs/03_calibration_targets.png",
       width = 8, height = 6)
ggsave(gg_targets,
       filename = "figs/03_calibration_targets.jpg",
       width = 8, height = 6)

#### 03.3 Run calibration algorithms ####
# Check that it works
v_params_calib <- c(r_DieMets = 0.042, # 0.03870286, 
                    r_RecurCDX2pos = (-log(0.80)/60),#0.003328773, 
                    hr_RecurCDX2neg = 2.73, # 3.601069078,
                    p_Mets  = 0.9)#0.980840626)
calibration_out(v_params_calib = v_params_calib, 
                l_params_all = l_params_all_calib)

#### 03.3.1 Specify calibration parameters ####
### Specify seed (for reproducible sequence of random numbers)
set.seed(1234)

### Number of random samples to obtain from the posterior distribution 
n_resamp <- 1000

### Names and number of input parameters to be calibrated
v_param_names  <- names(v_params_calib)
n_param        <- length(v_param_names)
## Labels for plotting
v_param_names_labels <- c("Monthly rate of metastatic\ndisease mortality",
                          "Montly rate of recurrence\nin CDX2-positive patients",
                          "Hazard ratio for developing\nrecurrence in CDX2-negative patients",
                          "Proportion of recurrences \nthat are metastatic")

### Vector with range on input search space
# Lower bound
v_lb <- c(r_DieMets       = 0.037, # O'Connell 2004 JNCI Stg IV Fig1 & Fig2;
          r_RecurCDX2pos  = 0.001,
          hr_RecurCDX2neg = 1.58, 
          p_Mets          = 0.9)  
# Upper bound
v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, # Rutter 2013 JNCI Table 4 5yr RS Colon cancer Stage IV 80+ Lower bound
          r_RecurCDX2pos  = 0.03,
          hr_RecurCDX2neg = 4.72, 
          p_Mets          = 0.99)  

### Number of calibration targets
v_target_names <- c("DFS CDX2neg",
                    "DFS CDX2pos",
                    "OS CDX2neg",
                    "OS CDX2pos",
                    "DSS CDX2neg",
                    "DSS CDX2pos")
n_target       <- length(v_target_names)

#### 03.3.2 Run IMIS algorithm ####
l_fit_imis <- IMIS::IMIS(B        =  1000,      # incremental sample size at each iteration of IMIS
                         B.re     =  n_resamp,  # desired posterior sample size
                         number_k =  10,        # maximum number of iterations in IMIS
                         D        =  0)
### Obtain posterior
m_calib_post         <- l_fit_imis$resample
v_p_DieMets_3yr      <- 1 - exp(-m_calib_post[, "r_DieMets"]*36)
v_p_RecurCDX2pos_3yr <- 1 - exp(-m_calib_post[, "r_RecurCDX2pos"]*36)

#### 03.4 Exploring posterior distribution ####
#### 03.4.1 Summary statistics of posterior distribution ####
### Compute posterior mean
v_calib_post_mean <- colMeans(m_calib_post)
p_DieMets_3yr_mean <- mean(v_p_DieMets_3yr)
p_RecurCDX2pos_3yr_mean <- mean(v_p_RecurCDX2pos_3yr)

### Compute posterior median and 95% credible interval
m_calib_post_95cr <- matrixStats::colQuantiles(m_calib_post, 
                                               probs = c(0.025, 0.5, 0.975))
p_DieMets_3yr_post_95cr <- quantile(v_p_DieMets_3yr, probs = c(0.025, 0.5, 0.975))
p_RecurCDX2pos_3yr_post_95cr <- quantile(v_p_RecurCDX2pos_3yr, probs = c(0.025, 0.5, 0.975))

### Compute posterior values for draw
v_calib_post <- exp(log_post(m_calib_post))

### Compute maximum-a-posteriori (MAP) as the mode of the sampled values
v_calib_post_map  <- m_calib_post[which.max(v_calib_post), ]
p_DieMets_3yr_map <- 1 - exp(-v_calib_post_map["r_DieMets"]*36)
p_RecurCDX2pos_3yr_map <- 1 - exp(-v_calib_post_map["r_RecurCDX2pos"]*36)

# Summary statistics
df_posterior_summ <- data.frame(
  Parameter = c(v_param_names_labels, 
                "3-year cumulative metastatic mortality risk", 
                "3-year cumulative recurrence risk"),
  Mean      = c(v_calib_post_mean, p_DieMets_3yr_mean, p_RecurCDX2pos_3yr_mean),
  rbind(m_calib_post_95cr, 
        p_DieMets_3yr_post_95cr,
        p_RecurCDX2pos_3yr_post_95cr),
  MAP = c(v_calib_post_map, p_DieMets_3yr_map, p_RecurCDX2pos_3yr_map),
  check.names = FALSE)
df_posterior_summ

### Save summary statistics of posterior distribution
## As .RData
save(df_posterior_summ, 
     file = "data/03_summary_posterior.RData")
## As .csv
write.csv(df_posterior_summ, 
          file = "tables/03_summary_posterior.csv", 
          row.names = FALSE)

#### 03.4.2 Visualization of posterior distribution ####
### Rescale posterior to plot density of plots
v_calib_alpha <- scales::rescale(v_calib_post)
df_calib_post <- data.frame(m_calib_post)
colnames(df_calib_post) <- v_param_names_labels

### Plot the 1000 draws from the posterior
gg_post_pairs_corr <- GGally::ggpairs(data.frame(m_calib_post),
                              upper = list(continuous = wrap("cor",
                                                             color = "black",
                                                             size = 5)),
                              diag = list(continuous = wrap("barDiag",
                                                            alpha = 0.8)),
                              lower = list(continuous = wrap("points", 
                                                             alpha = 0.3,
                                                             size = 0.7)),
                              columnLabels = v_param_names_labels
                              # columnLabels = c("mu[DieMets]",
                              #                  "mu[recurCDX2pos]",
                              #                  "hr[recurCDX2neg]",
                              #                  "p[Mets]"),
                              # labeller = "label_parsed"
                              ) +
  theme_bw(base_size = 18) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(hjust = 0))
gg_post_pairs_corr
ggsave(filename = "figs/03_posterior-IMIS-correlation-1k.pdf",
       width = 12, height = 12)
ggsave(filename = "figs/03_posterior-IMIS-correlation-1k.jpeg",
       width = 12, height = 12)
ggsave(filename = "figs/03_posterior-IMIS-correlation-1k.png",
       width = 12, height = 12)

### Plot the 1000 draws from the posterior with marginal histograms
m_samp_prior <- sample.prior(n_resamp)
df_samp_prior <- reshape2::melt(cbind(PDF = "Prior", 
                                      as.data.frame(m_samp_prior)), 
                                variable.name = "Parameter")
df_samp_post_imis <- reshape2::melt(cbind(PDF = "Posterior IMIS",
                                as.data.frame(m_calib_post)),
                          variable.name = "Parameter")
df_samp_prior_post <- dplyr::bind_rows(df_samp_prior, 
                                       df_samp_post_imis)
# df_samp_prior_post$Parameter <- factor(df_samp_prior_post$Parameter, 
#                                        levels = levels(df_samp_prior_post$Parameter),
#                                        ordered = TRUE, 
#                                        labels = c(expression(mu[DieMets]),
#                                                   expression(mu[recurCDX2pos]),
#                                                   expression(hr[recurCDX2neg]),
#                                                   expression(p[Mets])
#                                                    ))
gg_post_imis <- ggplot(df_samp_prior_post, 
                       aes(x = value, y = ..density.., fill = PDF)) +
  facet_wrap(~Parameter, scales = "free", 
             ncol = 4,
             labeller = label_parsed) +
  scale_x_continuous(n.breaks = 4) +
  geom_density(alpha = 0.5) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y =element_blank())
gg_post_imis

ggsave(gg_post_imis,
       filename = "figs/03_posterior_v_prior-IMIS-1k.pdf",
       width = 10, height = 6)
ggsave(gg_post_imis,
       filename = "figs/03_posterior_v_prior-IMIS-1k.png",
       width = 10, height = 6)
ggsave(gg_post_imis,
       filename = "figs/03_posterior_v_prior-IMIS-1k.jpg",
       width = 10, height = 6)

#### 03.5 Store posterior and MAP from IMIS calibration ####
save(m_calib_post,
     v_calib_post_mean,
     v_calib_post_map,
     file = "output/03_imis_output.RData")