## code to prepare `l_calibration_targets` dataset goes here
v_targets <- c(0.51, # DFS CDX2neg, pg.  pg. 219
               0.80, # DFS CDX2pos, pg.  pg. 219
               0.40, # OS CDX2neg, pg. Appendix pg. 28
               0.70, # OS CDX2pos, pg. Appendix pg. 28
               0.66, # DSS CDX2neg, pg. Appendix pg. 28
               0.89  # DSS CDX2pos, pg. Appendix pg. 28
               )
               
v_target_n <- c(6,  # DFS CDX2neg, pg.  pg. 219
                64, # DFS CDX2pos, pg.  pg. 219
                6,  # OS CDX2neg, pg. Appendix pg. 28
                70, # OS CDX2pos, pg. Appendix pg. 28
                66, # DSS CDX2neg, pg. Appendix pg. 28
                89  # DSS CDX2pos, pg. Appendix pg. 28
)

df_calibration_targets <- data.frame(Source = "Calibration target",
                                     Outcome = rep(c("DFS", "OS", "DSS"), each = 2),
                                     CDX2 = rep(c("CDX2-Negative", "CDX2-Positive"), 3), 
                                     Time = 60,
                                     S = v_targets,
                                     N = v_target_n)
df_calibration_targets <- df_calibration_targets %>%
  mutate(se = sqrt((S*(1-S))/N),
         lb = S - 1.96*se,
         ub = S + 1.96*se)
# Create .rda object for initial set of parameters and store it in 'data' folder
usethis::use_data(df_calibration_targets, overwrite = TRUE)
