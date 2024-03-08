#' Generate PSA dataset of CEA parameters
#'
#' \code{generate_psa_params} generates PSA input dataset by sampling decision 
#' model parameters from their distributions. The sample of the calibrated
#' parameters is a draw from their posterior distribution obtained with the
#' IMIS algorithm.
#' @param l_params_all List with all parameters of cost-effectiveness model.
#' @param seed Seed for reproducibility of Monte Carlo sampling.
#' @return 
#' A data frame with 18 columns of parameters for PSA. Each row is a parameter 
#' set sampled from distributions that characterize their uncertainty
#' @examples 
#' generate_psa_params(l_params_all = load_all_params())
#' @import doParallel
#' @export
generate_psa_params <- function(l_params_all, seed = 20210202){ # User defined
  with(as.list(l_params_all), {
  ## Load calibrated parameters
  n_sim <- nrow(m_calib_post)
  set_seed <- seed
  ## Obtain parameter bounds
  l_bounds <- generate_params_bounds(l_params_all)
  
  df_psa_params <- data.frame(
    ### Calibrated parameters
    m_calib_post,
    
    ### External parameters
    ## Proportion of CDX2-negative patients obtained from Step 3 of Figure 1 in page 213
    p_CDX2neg = rbeta(n_sim, (23+25), (389+232)), 
    ## Hazard ratio for disease recurrence among patients with CDX2-negative 
    # under chemo versus CDX2-negative patients without chemotherapy. From:
    # André et al. JCO 2015 Table 1, Stage III DFS: 0.79 [0.67, 0.94]
    # hr_Recurr_CDXneg_Rx = exp(rnorm(n_sim, log(0.79), sd = (log(0.94)-log(0.670))/(2*1.96))), OLD VALUE
    hr_Recurr_CDXneg_Rx = exp(rnorm(n_sim, log(0.82), sd = (log(1.08)-log(0.63))/(2*2.576))), # 2.576 is the Z value for a 1% significance level
    # Hazard ratio for disease recurrence among patients with CDX2-positive 
    # under chemo versus CDX2-positive patients without chemotherapy. 
    hr_Recurr_CDXpos_Rx = exp(rnorm(n_sim, log(0.99), sd = (log(0.999)-log(0.95))/(2*1.96))),
    # hr_Recurr_CDXpos_Rx = logitnorm::invlogit(rnorm(n_sim, logitnorm::logit(0.999), 
    #                                                 sd = (logitnorm::logit(0.999)-logitnorm::logit(0.95))/(2*1.96))),
    
    ### State rewards
    ## Costs
    # Cost of chemotherapy
    c_Chemo = rnorm(n_sim, 1391, sd = l_bounds$v_se$c_Chemo), 
    # Cost of chemotherapy administration
    c_ChemoAdmin = rnorm(n_sim, 315, sd = l_bounds$v_se$c_ChemoAdmin),
    # Initial costs in CRC Stage II (minus chemo and chemo admin) inflated from 2004 USD to 2020 USD using price index from PCE
    c_CRCStg2_init = rnorm(n_sim, c_CRCStg2_init, sd = 339*inf_pce/n_cycles_year), 
    # Continuing costs in CRC Stage II inflated from 2004 USD to 2020 USD using price index from PCE
    c_CRCStg2_cont = rnorm(n_sim, c_CRCStg2_cont, sd = 79*inf_pce/n_cycles_year),
    # Continuing costs in CRC Stage IV inflated from 2004 USD to 2020 USD using price index from PCE
    c_CRCStg4_cont = rnorm(n_sim, c_CRCStg4_cont, sd = 437*inf_pce/n_cycles_year),
    # Increase in cost when dying from cancer while in Stage II inflated from 2004 USD to 2020 USD using price index from PCE
    ic_DeathCRCStg2 = rnorm(n_sim, ic_DeathCRCStg2, sd = 705*inf_pce/n_cycles_year),
    # Increase in cost when dying from Other Causes (OC) while in Stage II inflated from 2004 USD to 2020 USD using price index from PCE
    ic_DeathOCStg2  = rnorm(n_sim, ic_DeathOCStg2, sd = 755*inf_pce/n_cycles_year),
    # Cost of IHC staining
    c_Test = runif(n_sim,
                   min = l_bounds$v_lb$c_Test, 
                   max = l_bounds$v_ub$c_Test),
    ## Utilities
    # Stage II without chemotherapy
    u_Stg2 = rnorm(n_sim, mean = u_Stg2, sd = l_bounds$v_se$u_Stg2),
    # Stage II with chemotherapy
    u_Stg2Chemo = rnorm(n_sim, mean = u_Stg2Chemo, sd = l_bounds$v_se$u_Stg2Chemo),
    u_Mets = rnorm(n_sim, mean = u_Mets, sd = l_bounds$v_se$u_Mets)
  )
  return(df_psa_params)
  }
  )

}

#' Run a probabilistic sensitivity analysis (ProbSA) of the cost-effectiveness
#' model
#'
#' \code{run_probsa} runs a probabilistic sensitivity analysis (ProbSA) and 
#' calculates cost and effectiveness outcomes.
#' @param df_psa_input Data frame with ProbSA input dataset .
#' @param n_str Number of strategies
#' @param parallel Run ProbSA in parallel
#' @return 
#' A list containing ProbSA cost and effectiveness outcomes for each strategy
#' @examples 
#' df_psa_input <- generate_psa_params(load_all_params())
#' run_probsa(df_psa_input, parallel = FALSE)
#' @export
run_probsa <- function(df_psa_input, n_str = 2, parallel = FALSE){
  ## Get number of simulations
  n_sim <- nrow(df_psa_input)
  if (parallel){
    ## Get OS
    os <- get_os()
    no_cores <- parallel::detectCores() - 1
    
    print(paste0("Parallelized PSA on ", os, " using ", no_cores, " cores"))
    
    n_time_init_psa <- Sys.time()
    
    ## Run parallelized PSA based on OS
    if(os == "macosx"){
      # Initialize cluster object
      cl <- parallel::makeForkCluster(no_cores)
      # Register clusters
      doParallel::registerDoParallel(cl)
      # Run parallelized PSA
      df_ce <- foreach::foreach(i = 1:n_sim, .combine = rbind) %dopar% { # i <- 1
        l_psa_input <- update_param_list(l_params_all, df_psa_input[i, ])
        l_out_temp <- calculate_ce_out(l_psa_input)
        df_ce <- c(l_out_temp$Cost, l_out_temp$Effect)
      }
      # Extract costs and effects from the PSA dataset
      df_c <- data.frame(df_ce[, 1:n_str])
      df_e <- data.frame(df_ce[, (n_str+1):(2*n_str)])
      # Register end time of parallelized PSA
      n_time_end_psa <- Sys.time()
    }
    if(os == "windows"){
      # Initialize cluster object
      cl <- parallel::makeCluster(no_cores)
      # Register clusters
      doParallel::registerDoParallel(cl)
      opts <- list(attachExportEnv = TRUE)
      # Run parallelized PSA
      df_ce <- foeach::foreach(i = 1:n_samp, .combine = rbind,
                               .export = ls(globalenv()),
                               .packages=c("dampack"),
                               .options.snow = opts) %dopar% {
                                 l_psa_input <- update_param_list(l_params_all,
                                                                  df_psa_input[i, ])
                                 l_out_temp <- calculate_ce_out(l_psa_input)
                                 df_ce <- c(l_out_temp$Cost, l_out_temp$Effect)
                               }
      # Extract costs and effects from the PSA dataset
      df_c <- data.frame(df_ce[, 1:n_str])
      df_e <- data.frame(df_ce[, (n_str+1):(2*n_str)])
      # Register end time of parallelized PSA
      n_time_end_psa <- Sys.time()
    }
    if(os == "linux"){
      # Initialize cluster object
      cl <- parallel::makeCluster(no_cores)
      # Register clusters
      doParallel::registerDoMC(cl)
      # Run parallelized PSA
      df_ce <- foreach::foreach(i = 1:n_sim, .combine = rbind) %dopar% {
        l_out_temp <- calculate_ce_out(df_psa_input[i, ])
        df_ce <- c(l_out_temp$Cost, l_out_temp$Effect)
      }
      # Extract costs and effects from the PSA dataset
      df_c <- data.frame(df_ce[, 1:n_str])
      df_e <- data.frame(df_ce[, (n_str+1):(2*n_str)])
      # Register end time of parallelized PSA
      n_time_end_psa <- Sys.time()
    }
    # Stope clusters
    stopCluster(cl)
    n_time_total_psa <- n_time_end_psa - n_time_init_psa
    print(paste0("PSA with ", scales::comma(n_sim), 
                 " simulations run in parallel on ", no_cores," cores in ",
                 round(n_time_total_psa, 2), " ",
                 units(n_time_total_psa)))
    l_out_probsa <- list(Costs = df_c,
                         Effects = df_e)
  } else{
    n_time_init_psa_series <- Sys.time()
    ### Initialize matrices for PSA output 
    ## Matrix of costs
    df_c <- as.data.frame(matrix(0, 
                                 nrow = n_sim,
                                 ncol = n_str))
    colnames(df_c) <- v_names_str
    ## Matrix of effectiveness
    df_e <- as.data.frame(matrix(0, 
                                 nrow = n_sim,
                                 ncol = n_str))
    colnames(df_e) <- v_names_str
    for(i in 1:n_sim){ # i <- 1
      l_psa_input <- update_param_list(l_params_all, df_psa_input[i,])
      df_out_temp <- calculate_ce_out(l_psa_input)
      df_c[i, ] <- df_out_temp$Cost
      df_e[i, ] <- df_out_temp$Effect
      # Display simulation progress
      if(i/(n_sim/100) == round(i/(n_sim/100),0)) {
        cat('\r', paste(i/n_sim * 100, "% done", sep = " "))
      }
    }
    n_time_end_psa_series <- Sys.time()
    n_time_total_psa_series <- n_time_end_psa_series - n_time_init_psa_series
    print(paste0("PSA with ", scales::comma(n_sim), " simulations run in series in ", 
                 round(n_time_total_psa_series, 2), " ", 
                 units(n_time_total_psa_series)))
    l_out_probsa <- list(Costs = df_c,
                         Effects = df_e)
  }
  return(l_out_probsa)
}