#' Base-case initial parameter set
#'
#' \code{load_params_init} generates the initial values of the CDX2 CEA model
#' 
#' @param n_age_init Initial age of the cohort.
#' @param n_age_max Oldest age of the cohort.
#' @param n_cycles_year Number of cycles per year
#' @param d_c Discount factor for costs
#' @param d_e Discount factor for effectiveness
#' @param index_pce Personal consumption expenditures (PCE) price index
#' @param p_CDX2neg Proportion of CDX2-negative patients
#' @param r_DieMets Cancer mortality rate
#' @param r_RecurCDX2pos Rate of recurrence in CDX2 positive patients
#' @param hr_RecurCDX2neg Hazard ratio of recurrence in CDX2 negative vs 
#' positive patients
#' @param hr_Recurr_CDXneg_Rx Hazard ratio for disease recurrence among patients 
#' with CDX2-negative under chemotherapy versus CDX2-negative patients without 
#' chemotherapy.
#' @param hr_Recurr_CDXpos_Rx Hazard ratio for disease recurrence among patients 
#' with CDX2-positive under chemotherapy versus CDX2-positive patients without 
#' chemotherapy.
#' @param p_Mets Proportion of recurrence being metastatic
#' @param c_Chemo Cost of chemotherapy
#' @param c_ChemoAdmin Cost of chemotherapy administration
#' @param c_CRCStg2_init Initial costs in CRC Stage II (minus chemo and 
#' chemotherapy administration) 
#' @param c_CRCStg2_cont Continuing costs in CRC Stage II
#' @param c_CRCStg4_cont Continuing costs in CRC Stage IV
#' @param ic_DeathCRCStg2 Increase in cost when dying from cancer while in 
#' Stage II
#' @param ic_DeathOCStg2 Increase in cost when dying from Other Causes (OC) 
#' while in Stage II
#' @param c_Test Cost of IHC staining
#' @param u_Stg2 Utility for CRC Stage II patients
#' @param u_Stg2Chemo Utility for CRC Stage II patients under chemotherapy
#' @param u_Mets Utility for metastatic recurrence state
#' @return 
#' List of all parameters 
#' @export
load_params_init <- function(
  # Initial and final ages
  n_age_init = 65,
  n_age_max  = 100,
  # Number of cycles per year
  n_cycles_year = 12,
  # Discount factors
  d_c = 0.03,
  d_e = 0.03,
  # Personal consumption expenditures (PCE) price index to inflate cancer costs
  index_pce = 0.018,
  # Proportion of CDX2-negative patients obtained from Step 3 of Figure 1 in page 213
  p_CDX2neg = 0.07174887892376682, # (23+25)/((23+25) + (389+232))
  # Proportion of recurrence being metastatic (CALIBRATED)
  p_Mets  = 0.980840626,
  # Cancer mortality rate (CALIBRATED)
  r_DieMets = 0.03870286,
  # Rate of recurrence in CDX2 positive patients (CALIBRATED)
  r_RecurCDX2pos = 0.003328773,
  # Hazard ratio of recurrence in CDX2 negative vs positive patients (CALIBRATED)
  hr_RecurCDX2neg = 3.601069078,
  # # Hazard ratio for disease recurrence among patients with CDX2-negative 
  # # under chemo versus CDX2-negative patients without chemotherapy. From:
  # # André et al. JCO 2015 Table 1, Stage III DFS: 0.79 [0.67, 0.94]
  # hr_Recurr_CDXneg_Rx = 0.79, 
  ## Hazard ratio for disease recurrence among patients with CDX2-negative
  # under chemo versus CDX2-negative patients without chemotherapy. From:
  # QUASAR. Lancet 2007 Figure 3, Stage II RR [99% CI]: 0.82 [0.63, 1.08]
  hr_Recurr_CDXneg_Rx = 0.82,
  # Hazard ratio for disease recurrence among patients with CDX2-positive 
  # under chemo versus CDX2-positive patients without chemotherapy. From: [TO BE ADDED]
  hr_Recurr_CDXpos_Rx = 1.00, 
  ### State rewards
  ## Costs
  # Cost of chemotherapy
  c_Chemo =  1576,
  # Cost of chemotherapy administration
  c_ChemoAdmin = 315, 
  # Initial costs in CRC Stage II (minus chemo and chemo admin) in 2004 USD
  c_CRCStg2_init = (32039 - (1391+315)),
  # Continuing costs in CRC Stage II in 2004 USD
  c_CRCStg2_cont = 1722,
  # Continuing costs in CRC Stage IV in 2004 USD
  c_CRCStg4_cont = 7629,
  # Increase in cost when dying from cancer while in Stage II in 2004 USD
  ic_DeathCRCStg2 = 41500,
  # Increase in cost when dying from Other Causes (OC) while in Stage II in 2004 USD
  ic_DeathOCStg2  = 8969,
  # Cost of IHC staining
  c_Test = 112,
  ## Utilities
  u_Stg2 = 0.74,      # Ness 1999, Outcome state "A" from table 3
  u_Stg2Chemo = 0.67, # Ness 1999, Outcome state "BC" from table 4
  u_Mets = 0.25       # Ness 1999, Outcome state "FG" from table 3
){
  # Number of cycles
  n_cycles <- (n_age_max - n_age_init)*n_cycles_year # Time horizon, number of monthly cycles
  # Inflation factor based on PCE from 2004 USD to 2020 USD
  inf_pce <- (1 + index_pce)^16
  # Inflate costs
  c_Chemo        <- c_Chemo*(1 + index_pce)^2 # Cost of chemotherapy
  c_ChemoAdmin   <- c_ChemoAdmin*(1 + index_pce)^2  # Cost of chemotherapy administration
  c_CRCStg2_init <- (c_CRCStg2_init*inf_pce)/n_cycles_year # Initial costs in CRC Stage II (minus chemo and chemo admin) inflated from 2004 USD to 2018 USD using price index from PCE
  c_CRCStg2_cont <- (c_CRCStg2_cont*inf_pce)/n_cycles_year # Continuing costs in CRC Stage II inflated from 2004 USD to 2018 USD using price index from PCE
  c_CRCStg4_cont <- (c_CRCStg4_cont*inf_pce)/n_cycles_year # Continuing costs in CRC Stage IV inflated from 2004 USD to 2018 USD using price index from PCE
  ic_DeathCRCStg2 <- ic_DeathCRCStg2*inf_pce # 92851, # Increase in cost when dying from cancer while in Stage II inflated from 2004 USD to 2018 USD using price index from PCE
  ic_DeathOCStg2  <- ic_DeathOCStg2*inf_pce # Increase in cost when dying from Other Causes (OC) while in Stage II inflated from 2004 USD to 2018 USD 
  c_Test          <- c_Test*(1 + index_pce)^2 # Cost of IHC staining
  ### Create list of initial parameters
  l_params_init <- list(
    # Initial and final ages
    n_age_init = n_age_init,
    n_age_max  = n_age_max,
    # Number of cycles
    n_cycles = n_cycles,
    # Inflation factor based on PCE
    inf_pce = inf_pce,
    # Number of cycles per year
    n_cycles_year = n_cycles_year,
    # Discount factors
    d_c = d_c,
    d_e = d_e,
    # Personal consumption expenditures (PCE) price index
    index_pce = index_pce,
    # Disease parameters
    p_CDX2neg = p_CDX2neg,
    r_DieMets = r_DieMets, 
    r_RecurCDX2pos  = r_RecurCDX2pos, 
    hr_RecurCDX2neg = hr_RecurCDX2neg,
    hr_Recurr_CDXneg_Rx = hr_Recurr_CDXneg_Rx, 
    hr_Recurr_CDXpos_Rx = hr_Recurr_CDXpos_Rx, 
    p_Mets  = p_Mets,
    # Costs
    c_Chemo = c_Chemo,
    c_ChemoAdmin = c_ChemoAdmin,
    c_CRCStg2_init = c_CRCStg2_init,
    c_CRCStg2_cont = c_CRCStg2_cont,
    c_CRCStg4_cont = c_CRCStg4_cont,
    ic_DeathCRCStg2 = ic_DeathCRCStg2,
    ic_DeathOCStg2  = ic_DeathOCStg2,
    c_Test = c_Test,
    # Utilities
    u_Stg2 = u_Stg2,
    u_Stg2Chemo = u_Stg2Chemo,
    u_Mets = u_Mets
  )
  return(l_params_init)
}

#' Load mortality data
#'
#' \code{load_mort_data} is used to load age-specific mortality from .csv file 
#' into vector.
#'
#' @param file String with the location and name of the file with mortality 
#' data. If \code{NULL}, \code{v_r_mort_by_age} will be used as default
#' @return 
#' A vector with mortality by age.
#' @export
load_mort_data <- function(file = NULL){
  # Load mortality data from file
  if(!is.null(file)) {
    df_r_mort_by_age <- read.csv(file = file)}
  else{
    df_r_mort_by_age <- all_cause_mortality
  }
  # Vector with mortality rates
  v_r_mort_by_age  <- dplyr::select(df_r_mort_by_age, 
                                    .data$Age, .data$Total)
  
  return(v_r_mort_by_age)
}

#' Load all parameters
#'
#' \code{load_all_params} loads all parameters for the decision model from multiple sources and creates a list.
#'
#' @param file.init String with the location and name of the file with initial set of parameters
#' @param file.mort String with the location and name of the file with mortality data
#' @return 
#' A list of all parameters used for the decision model.
#' @export
load_all_params <- function(l_params_init = NULL,
                            file_init = NULL,
                            file_mort = NULL){ # User defined
  #### Load initial set of initial parameters from .csv file ####
  if(is.null(l_params_init)) {
    l_params_init <- load_params_init()
  } else{
    l_params_init <- l_params_init
  }
  
  #### All-cause age-specific mortality from .csv file ####
  v_r_mort_by_age <- load_mort_data(file = file_mort)
  
  l_params_all <- with(as.list(l_params_init), {
    #### General setup ####
    v_names_str <- c("No Treat", "Test & treat")# CEA strategies
    n_str       <- length(v_names_str) # Number of strategies
    v_age_names <- paste(rep(n_age_init:(n_age_max-1), each = n_cycles_year), 
                         1:n_cycles_year, 
                         sep = ".")
    # Vector with the 6 health states of the model
    v_names_states <- c("CDX2pos", "CDX2neg",
                        "Mets", "Dead_OC", "Dead_C")
    n_states <- length(v_names_states) # number of health states
    
    # Within-cycle correction (WCC) using Simpson's 1/3 rule
    v_wcc <- darthtools::gen_wcc(n_cycles = n_cycles, 
                                 # method = "Simpson1/3") # vector of wcc
                                 method = "half-cycle") # vector of wcc
    
    # Filter for selected ages
    v_r_mort_by_age <- v_r_mort_by_age %>%
      filter(Age >= n_age_init & Age < n_age_max) %>%
      dplyr::select(Total) %>%
      as.matrix()
    # Compute monthly mortality rates 
    v_r_mort_by_age_month <- rep(v_r_mort_by_age, 
                                 each = n_cycles_year)/n_cycles_year
    
    #### Create list with all parameters ####
    l_params_all <- list(
      v_names_str = v_names_str,
      n_str       = n_str,
      n_age_init  = n_age_init, 
      n_cycles    = n_cycles, 
      v_age_names = v_age_names,
      v_names_states = v_names_states,
      n_states = n_states,
      v_r_mort_by_age_month = v_r_mort_by_age_month,
      v_wcc = v_wcc
    )
    return(l_params_all)
  }
  )
  
  l_params_all <- c(l_params_all, 
                    l_params_init) # Add initial set of parameters
  
  return(l_params_all)
}

#' Update parameters
#'
#' \code{update_param_list} is used to update list of all parameters with new 
#' values for specific parameters.
#'
#' @param l_params_all List with all parameters of decision model
#' @param params_updated Parameters for which values need to be updated
#' @return 
#' A list with all parameters updated.
#' @export
update_param_list <- function(l_params_all, params_updated){
  
  if (typeof(params_updated)!="list"){
    params_updated <- split(unname(params_updated),names(params_updated)) #converte the named vector to a list
  }
  l_params_all <- modifyList(l_params_all, params_updated) #update the values
  return(l_params_all)
}