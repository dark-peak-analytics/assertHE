#' Generate model outputs for calibration from a parameter set
#'
#' \code{calibration_out} computes model outputs to be used for calibration 
#' routines.
#'
#' @param v_params_calib Vector of parameters that need to be calibrated.
#' @param l_params_all List with all parameters of the decision model.
#' @return 
#' A list with disease-free survival (DFS), disease-specific survival (DSS), and
#' overall survival (OS)
#' @export
calibration_out <- function(v_params_calib, l_params_all){ # User defined
  # Substitute values of calibrated parameters in base-case with 
  # calibrated values
  l_params_all <- update_param_list(l_params_all = l_params_all, params_updated = v_params_calib)
  
  (l_params_all[names(v_params_calib)])
  
  ## Run model with updated calibrated parameters by CDX2 status
  # For CDX2-negative patients
  l_out_stm_CDX2neg <- decision_model(l_params_all = l_params_all, 
                                      p_CDX2neg_init = 1, Trt = FALSE)
  # For CDX2-positive patients
  l_out_stm_CDX2pos <- decision_model(l_params_all = l_params_all, 
                                      p_CDX2neg_init = 0, Trt = FALSE)
  
  ####### Epidemiological Output ###########################################
  #### Disease-Free Survival (DFS) ####
  # Definition based on: https://www.cancer.gov/publications/dictionaries/cancer-terms/def/disease-free-survival
  v_dfs_CDX2neg <- rowSums(l_out_stm_CDX2neg$m_M[, c("CDX2neg", "Dead_OC")])
  v_dfs_CDX2pos <- rowSums(l_out_stm_CDX2pos$m_M[, c("CDX2pos", "Dead_OC")])
  
  #### Overall Survival (OS) ####
  v_os_CDX2neg <- rowSums(l_out_stm_CDX2neg$m_M[, c("CDX2neg", "Mets")])
  v_os_CDX2pos <- rowSums(l_out_stm_CDX2pos$m_M[, c("CDX2pos", "Mets")])
  
  #### Disease-Specific Survival (DSS) ####
  # Definition based on: https://www.cancer.gov/publications/dictionaries/cancer-terms/def/disease-specific-survival-rate
  v_dss_CDX2neg <- rowSums(l_out_stm_CDX2neg$m_M[, c("CDX2neg", "Mets", "Dead_OC")])
  v_dss_CDX2pos <- rowSums(l_out_stm_CDX2pos$m_M[, c("CDX2pos", "Mets", "Dead_OC")])
  
  ####### Return Output ###########################################
  l_out <- list(v_dfs_CDX2neg = v_dfs_CDX2neg[61],
                v_dfs_CDX2pos = v_dfs_CDX2pos[61],
                v_os_CDX2neg  = v_os_CDX2neg[61],
                v_os_CDX2pos  = v_os_CDX2pos[61],
                v_dss_CDX2neg = v_dss_CDX2neg[61],
                v_dss_CDX2pos = v_dss_CDX2pos[61])
  return(l_out)
}

#' Sample from prior distributions of calibrated parameters
#'
#' \code{sample.prior} generates a sample of parameter sets from their prior 
#' distribution.
#' @param n_samp Number of samples.
#' @param v_param_names Vector with parameter names.
#' @param v_ub Vector with lower bounds for each parameter.
#' @param v_lb Vector with upper bounds for each parameter.
#' @return 
#' A matrix with 3 rows and \code{n_samp} rows. Each row corresponds to a 
#' parameter set sampled from their prior distributions
#' @examples 
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' sample.prior(2)
#' @export
sample.prior <- function(n_samp,
                         v_param_names = c("r_DieMets",
                                           "r_RecurCDX2pos",
                                           "hr_RecurCDX2neg",
                                           "p_Mets"),
                         v_lb = c(r_DieMets       = 0.037, # O'Connell 2004 JNCI Stg IV Fig1 & Fig2;
                                  r_RecurCDX2pos  = 0.001,
                                  hr_RecurCDX2neg = 1.58, 
                                  p_Mets          = 0.9),
                         v_ub = c(r_DieMets       = -log(1-(1-0.03))/60, # Rutter 2013 JNCI Table 4 5yr RS Colon cancer Stage IV 80+ Lower bound
                                  r_RecurCDX2pos  = 0.03,
                                  hr_RecurCDX2neg = 4.72, 
                                  p_Mets          = 0.99)){
  n_param <- length(v_param_names)

  ### Transformed design 
  ## Transformed bounds
  v_lb_transf <- c(log(v_lb[1:3]), logitnorm::logit(v_lb[4]))
  v_ub_transf <- c(log(v_ub[1:3]), logitnorm::logit(v_ub[4]))
  # Find means and SDs of logit-normal and log-normal based on bounds 
  # assuming bounds are represent the 95% equal tailed interval for these 
  # distributions
  v_mu_transf <- (v_ub_transf + v_lb_transf)/2
  v_sd_transf <- (v_ub_transf - v_lb_transf)/(2*2) # *1.96
  
  ### Draw LHS from Uniform[0,1] distributions
  m_lhs_unif   <- lhs::randomLHS(n = n_samp, k = n_param)
  colnames(m_lhs_unif) <- v_param_names
  
  ### Transformed LHS
  ## Get values in Normal scale
  m_lhs_normal <- m_lhs_unif
  for (i in 1:n_param){
    m_lhs_normal[, i] <- qnorm(m_lhs_unif[,i], v_mu_transf[i], v_sd_transf[i])
  }
  
  m_param_samp <- m_lhs_normal
  colnames(m_param_samp) <- v_param_names
  ## Get values in Original scale
  m_param_samp[, 1:3] <- exp(m_lhs_normal[, 1:3])
  m_param_samp[, 4]   <- logitnorm::invlogit(m_lhs_normal[, 4])
  
  return(m_param_samp)
}

#' Evaluate log-prior of calibrated parameters
#'
#' \code{log_prior} computes a log-prior value for one (or multiple) parameter 
#' set(s) based on their prior distributions.
#' @param v_params Vector (or matrix) of model parameters.
#' @param v_param_names Vector with parameter names.
#' @param v_ub Vector with lower bounds for each parameter.
#' @param v_lb Vector with upper bounds for each parameter.
#' @return 
#' A scalar (or vector) with log-prior values.
#' @examples 
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' log_prior(v_params = sample.prior(n_samp = 5))
#' @export
log_prior <- function(v_params, 
                      v_param_names = c("r_DieMets",
                                        "r_RecurCDX2pos",
                                        "hr_RecurCDX2neg",
                                        "p_Mets"),
                      v_lb = c(r_DieMets       = 0.037, # O'Connell 2004 JNCI Stg IV Fig1 & Fig2;
                               r_RecurCDX2pos  = 0.001,
                               hr_RecurCDX2neg = 1.58, 
                               p_Mets          = 0.9),
                      v_ub = c(r_DieMets       = -log(1-(1-0.03))/60, # Rutter 2013 JNCI Table 4 5yr RS Colon cancer Stage IV 80+ Lower bound
                               r_RecurCDX2pos  = 0.03,
                               hr_RecurCDX2neg = 4.72, 
                               p_Mets          = 0.99)){
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  colnames(v_params) <- v_param_names
  ## Number of parameters
  n_param <- length(v_param_names)
  ## Number of samples
  n_samp <- nrow(v_params)
  
  ### Transformed design 
  ## Transformed bounds
  v_lb_transf <- c(log(v_lb[1:3]), logitnorm::logit(v_lb[4]))
  v_ub_transf <- c(log(v_ub[1:3]), logitnorm::logit(v_ub[4]))
  # Find means and SDs of logit-normal and log-normal based on bounds 
  # assuming bounds are represent the 95% equal tailed interval for these 
  # distributions
  v_mu_transf <- (v_ub_transf + v_lb_transf)/2
  v_sd_transf <- (v_ub_transf - v_lb_transf)/(2*2) # *1.96
  
  lprior <- rep(0, n_samp)
  lprior <- lprior + dlnorm(v_params[, 1], v_mu_transf[1], v_sd_transf[1], log = T)
  lprior <- lprior + dlnorm(v_params[, 2], v_mu_transf[2], v_sd_transf[2], log = T)
  lprior <- lprior + dlnorm(v_params[, 3], v_mu_transf[3], v_sd_transf[3], log = T)
  lprior <- lprior + logitnorm::dlogitnorm(v_params[, 4], v_mu_transf[4], v_sd_transf[4], log = T)
  return(lprior)
}

#' Evaluate prior of calibrated parameters
#'
#' \code{prior} computes a prior value for one (or multiple) parameter set(s).
#' @param v_params Vector (or matrix) of model parameters 
#' @return 
#' A scalar (or vector) with prior values.
#' @examples
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' prior(v_params = sample.prior(n_samp = 5))
#' @export
prior <- function(v_params) { 
  v_prior <- exp(log_prior(v_params)) 
  return(v_prior)
}

#' Log-likelihood function for a parameter set
#'
#' \code{log_lik} computes a log-likelihood value for one (or multiple) 
#' parameter set(s).
#'
#' @param v_params Vector (or matrix) of model parameters.
#' @param l_params_all List with all parameters of the decision model. 
#' @return 
#' A scalar (or vector) with log-likelihood values.
#' @importFrom stats dnorm dunif quantile qunif rbeta rgamma sd
#' @examples 
#' \dontrun{
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' v_target_names <- c("DFS", "OS", "DSS")
#' n_target       <- length(v_target_names)
#' log_lik(v_params = sample.prior(n_samp = 2))
#' }
#' @export
log_lik <- function(v_params,
                    l_params_all = load_all_params()){ # User defined
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  
  n_samp <- nrow(v_params)
  v_target_names <- c("DFS", "OS", "DSS")
  n_target       <- length(v_target_names)
  v_llik <- matrix(0, nrow = n_samp, ncol = n_target) 
  colnames(v_llik) <- v_target_names
  v_llik_overall <- numeric(n_samp)
  for(j in 1:n_samp) { # j=1
    jj <- tryCatch( { 
      ###   Run model for parameter set "v_params" ###
      l_model_res <- calibration_out(v_params_calib = v_params[j, ], 
                                     l_params_all = l_params_all)
      
      ###  Calculate log-likelihood of model outputs to targets  ###
      ## TARGET 1: Disease-free survival ("DFS")
      ## Normal log-likelihood  
      v_llik[j, "DFS"] <- sum(dnorm(x = df_calibration_targets$S[1:2],
                                     mean = c(l_model_res$v_dfs_CDX2neg,
                                              l_model_res$v_dfs_CDX2pos),
                                     sd = df_calibration_targets$se[1:2],
                                     log = T))
      
      ## TARGET 2: Overall survival ("OS")
      ## Normal log-likelihood
      v_llik[j, "OS"] <- sum(dnorm(x = df_calibration_targets$S[3:4],
                                   mean = c(l_model_res$v_os_CDX2neg,
                                            l_model_res$v_os_CDX2pos),
                                   sd = df_calibration_targets$se[3:4],
                                   log = T))
      
      ## TARGET 3: Disease-specific survival ("DSS")
      ## Normal log-likelihood
      v_llik[j, "DSS"] <- sum(dnorm(x = df_calibration_targets$S[5:6],
                                    mean = c(l_model_res$v_dss_CDX2neg,
                                             l_model_res$v_dss_CDX2pos),
                                    sd = df_calibration_targets$se[5:6],
                                    log = T))
      
      ## OVERALL
      ## can give different targets different weights (user must change this)
      v_weights <- rep(1, n_target)
      ## weighted sum
      v_llik_overall[j] <- v_llik[j, ] %*% v_weights
    }, error = function(e) NA) 
    if(is.na(jj)) { v_llik_overall <- -Inf }
  } ## End loop over sampled parameter sets
  
  ## return GOF
  return(v_llik_overall)
}

#' Parallel evaluation of log-likelihood function for a sets of parameters
#'
#' \code{log_lik_par} computes a log-likelihood value for one (or multiple) 
#' parameter set(s) using parallel computation.
#'
#' @param v_params Vector (or matrix) of model parameters.
#' @param l_params_all List with all parameters of the decision model. 
#' @return 
#' A scalar (or vector) with log-likelihood values.
#' @importFrom stats dnorm dunif quantile qunif rbeta rgamma sd
#' @examples 
#' \dontrun{
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' v_target_names <- c("DFS", "OS", "DSS")
#' n_target       <- length(v_target_names)
#' log_lik_par(v_params = sample.prior(n_samp = 2))
#' }
#' @export
log_lik_par <- function(v_params, 
                        l_params_all = l_params_all_calib,
                        ...) { 
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  
  n_samp <- nrow(v_params)
  
  ### Get OS
  os <- get_os()
  
  no_cores <- parallel::detectCores() - 1
  
  print(paste0("Parallelized Likelihood calculations on ", os, " using ", no_cores, " cores"))
  
  n_time_init_likpar <- Sys.time()
  
  if(os == "macosx"){
    # Initialize cluster object
    cl <- parallel::makeForkCluster(no_cores) 
    doParallel::registerDoParallel(cl)
    v_llk <- foreach::foreach(i = 1:n_samp, .combine = c) %dopar% {
      log_lik(v_params[i, ], l_params_all) # i = 1
    }
    n_time_end_likpar <- Sys.time()
  }
  if(os == "windows"){
    # Initialize cluster object
    cl <- parallel::makeCluster(no_cores)
    doParallel::registerDoParallel(cl)
    opts <- list(attachExportEnv = TRUE)
    v_llk <- foreach::foreach(i = 1:n_samp, .combine = c,
                   .export = ls(globalenv()),
                   .packages=c(),
                   .options.snow = opts) %dopar% {
                     log_lik(v_params[i, ], ...)
                   }
    n_time_end_likpar <- Sys.time()
  }
  if(os == "linux"){
    # Initialize cluster object
    cl <- parallel::makeCluster(no_cores)
    doMC::registerDoMC(cl)
    v_llk <- foreach::foreach(i = 1:n_samp, .combine = c) %dopar% {
      log_lik(v_params[i, ], ...)
    }
    n_time_end_likpar <- Sys.time()
  }
  
  parallel::stopCluster(cl)
  n_time_total_likpar <- difftime(n_time_end_likpar, n_time_init_likpar, 
                                  units = "hours")
  print(paste0("Runtime: ", round(n_time_total_likpar, 2), " hrs."))
  #-# Try this: # PO
  rm(cl)        # PO
  gc()          # PO
  #-#           # PO
  return(v_llk)
}

#' Likelihood
#'
#' \code{likelihood} computes a likelihood value for one (or multiple) 
#' parameter set(s).
#'
#' @param v_params Vector (or matrix) of model parameters. 
#' @return 
#' A scalar (or vector) with likelihood values.
#' @examples
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' v_target_names <- c("DFS", "OS", "DSS")
#' n_target       <- length(v_target_names)
#' likelihood(v_params = sample.prior(n_samp = 2))
#' @export
likelihood <- function(v_params){ 
  v_like <- exp(log_lik_par(v_params)) 
  return(v_like)
}

#' Evaluate log-posterior of calibrated parameters
#'
#' \code{log_post} Computes a log-posterior value for one (or multiple) 
#' parameter set(s) based on the simulation model, likelihood functions and 
#' prior distributions.
#' @param v_params Vector (or matrix) of model parameters 
#' @return 
#' A scalar (or vector) with log-posterior values.
#' @examples 
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' v_target_names <- c("DFS", "OS", "DSS")
#' n_target       <- length(v_target_names)
#' log_post(v_params = sample.prior(n_samp = 5))
#' @export
log_post <- function(v_params) { 
  v_lpost <- log_prior(v_params) + log_lik_par(v_params)
  return(v_lpost) 
}

#' Evaluate posterior of calibrated parameters
#'
#' \code{posterior} computes a posterior value for one (or multiple) parameter 
#' set(s).
#' @param v_params Vector (or matrix) of model parameters 
#' @return 
#' A scalar (or vector) with posterior values.
#' @examples
#' \dontrun{
#' v_param_names  <- c("r_DieMets",
#'                     "r_RecurCDX2pos",
#'                     "hr_RecurCDX2neg",
#'                     "p_Mets")
#' n_param        <- length(v_param_names)
#' v_lb <- c(r_DieMets       = 0.037, 
#'           r_RecurCDX2pos  = 0.001,
#'           hr_RecurCDX2neg = 1.58, 
#'           p_Mets          = 0.9))  # lower bound
#' v_ub <- c(r_DieMets       = -log(1-(1-0.03))/60, 
#'           r_RecurCDX2pos  = 0.03,
#'           hr_RecurCDX2neg = 4.72, 
#'           p_Mets          = 0.99) # upper bound
#' v_target_names <- c("DFS", "OS", "DSS")
#'  n_target       <- length(v_target_names)
#'  posterior(v_params = sample.prior(n_samp = 5))
#' }
#' @export
posterior <- function(v_params) { 
  v_posterior <- exp(log_post(v_params)) 
  return(v_posterior)
}

#' Get operating system
#' 
#' @return 
#' A string with the operating system.
#' @export
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "MacOSX"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}