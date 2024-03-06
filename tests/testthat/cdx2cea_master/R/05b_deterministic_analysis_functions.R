#' Cost-effectiveness model
#'
#' \code{ce_model} implements the cost-effectiveness model used.
#'
#' @param l_params_all List with all parameters of cost-effectiveness model
#' @param p_CDX2neg_init Initial proportion of CDX2-negative patients. Default 
#' is NULL and will take the value of \code{p_CDX2neg} defined in 
#' \code{load_all_params()}
#' @param Trt Treatment variable (default is FALSE)
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default 
#' = FALSE.
#' @param verbose Logical variable to indicate print out of messages. Default 
#' = FALSE
#' @return 
#' The transition probability array, the cohort trace matrix, the transition 
#' dynamics array, and the undiscounted and discounted life years (LYs), 
#' quality-adjusted life years (QALYs) and costs
#' @export
ce_model <- function(l_params_all, p_CDX2neg_init = NULL, Trt = FALSE,
                     err_stop = TRUE, verbose = TRUE){ # User defined
  with(as.list(l_params_all), {
    ### Run decision model
    l_model_out <- decision_model(l_params_all = l_params_all, 
                                  p_CDX2neg_init = p_CDX2neg_init, 
                                  Trt = Trt, 
                                  err_stop = err_stop, verbose = verbose)
    a_P <- l_model_out$a_P
    m_M <- l_model_out$m_M
    a_A <- l_model_out$a_A
    
    ### Create discounting vectors
    v_dwc <- 1 / ((1 + d_c/n_cycles_year) ^ seq(0, n_cycles)) # vector with discount weights for costs
    v_dwe <- 1 / ((1 + d_e/n_cycles_year) ^ seq(0, n_cycles)) # vector with discount weights for QALYs
    
    #### State Rewards ####
    ### Life Years
    v_R_ly <- c(CDX2pos = 1/12,
                CDX2neg = 1/12,
                Mets    = 1/12,
                Dead_OC = 0,
                Dead_C  = 0)
    
    ### Utilities
    ## Utility for Stage 2 Colon Cancer
    v_u_S2 <- c(rep(u_Stg2Chemo/12, 6), 
                rep(u_Stg2/12, (n_cycles - 6 + 1))) * Trt + # If on chemotherapy
      rep(u_Stg2/12, n_cycles + 1) * (1 - Trt)# If not on chemotherapy
    ## Utility for Mets
    v_u_Mets <- rep(u_Mets/12, n_cycles + 1)
    ## utility when Dead
    u_D <- 0
    ## Array of time-dependent state utilities
    ## Initialize array
    a_R_u <- array(NaN, dim = c(n_states, n_states, (n_cycles + 1)),
                 dimnames = list(v_names_states, v_names_states, 0:n_cycles))
    ## Fill in array
    # In CDX2 positive (One alternative is to manually assign utilities to each exiting state)
    a_R_u["CDX2pos","CDX2pos", ]  <- v_u_S2
    a_R_u["CDX2neg","CDX2pos", ]  <- v_u_S2
    a_R_u["Mets", "CDX2pos", ]    <- v_u_S2
    a_R_u["Dead_OC", "CDX2pos", ] <- v_u_S2
    a_R_u["Dead_C", "CDX2pos", ]  <- v_u_S2
    # In CDX2 negative (Another alternative is to use `rep`)
    a_R_u[, "CDX2neg", ] <- rep(v_u_S2, each = n_states)
    # In Mets Recurrence
    a_R_u[, "Mets", ] <- rep(v_u_Mets, each = n_states)
    # In Dead OC
    a_R_u[, "Dead_OC", ] <- rep(u_D, each = n_states)
    # In Dead C
    a_R_u[, "Dead_C", ] <- rep(u_D, each = n_states)
    
    ### Costs
    ## Initial and continuing Costs in Stage II (no evidence of disease, NED)
    v_c_S2 <- c(rep(c_CRCStg2_init, 12), rep(c_CRCStg2_cont, ((n_cycles + 1) - 12)))
    ## Costs of Mets 
    c_Mets <- c_CRCStg4_cont
    ## Cost when Dead
    c_D <- 0
    ### Array of age-dependent state utilities
    ## Initialize array
    a_R_c <- array(NaN, dim = c(n_states, n_states, (n_cycles + 1)),
                 dimnames = list(v_names_states, v_names_states, 0:n_cycles))
    ## Fill in array
    # In CDX2 positive (One alternative is to manually assign Utilities to each exiting state)
    a_R_c["CDX2pos","CDX2pos", ]  <- v_c_S2
    a_R_c["CDX2neg","CDX2pos", ]  <- v_c_S2
    a_R_c["Mets", "CDX2pos", ]    <- v_c_S2
    a_R_c["Dead_OC", "CDX2pos", ] <- v_c_S2
    a_R_c["Dead_C", "CDX2pos", ]  <- v_c_S2
    # In CDX2 negative (Another alternative is to use `rep`)
    a_R_c[, "CDX2neg", ] <- rep(v_c_S2,  each = n_states) # Or: rep(v_c_S2, each = n.s), if v_c_S2 is time dependent
    # In Mets Recurrence
    a_R_c[, "Mets", ] <- c_Mets
    # In Dead OC
    a_R_c[, "Dead_OC", ] <- c_D
    # In Dead C
    a_R_c[, "Dead_C", ] <- c_D
    
    #### Transition rewards ####
    ## Add increment in cost due to transition from CDX2 or Mets to Dead_OC
    a_R_c["CDX2pos", "Dead_OC", ] <- a_R_c["CDX2pos", "Dead_OC", ] + ic_DeathOCStg2
    a_R_c["CDX2neg", "Dead_OC", ] <- a_R_c["CDX2neg", "Dead_OC", ] + ic_DeathOCStg2
    a_R_c["Mets", "Dead_OC", ]    <- a_R_c["Mets", "Dead_OC", ]    + ic_DeathOCStg2
    ## Add increment in cost due to transition from CDX2 or Mets to Dead_OC
    a_R_c["CDX2pos", "Dead_C", ] <- a_R_c["CDX2pos", "Dead_C", ] + ic_DeathCRCStg2
    a_R_c["CDX2neg", "Dead_C", ] <- a_R_c["CDX2neg", "Dead_C", ] + ic_DeathCRCStg2
    a_R_c["Mets", "Dead_C", ]    <- a_R_c["Mets", "Dead_C", ]    + ic_DeathCRCStg2
    
    #### Expected QALYs and Costs for all transitions per cycle ####
    a_Y_c <- a_A * a_R_c
    a_Y_u <- a_A * a_R_u 
    
    #### Expected LYS, QALYs and Costs per cycle ####
    ## Vector of LYs, QALYs and Costs
    v_ly   <- m_M %*% v_R_ly
    v_qaly <- apply(a_Y_u, 3, sum) # sum the proportion of the cohort across transitions 
    v_cost <- apply(a_Y_c, 3, sum) # sum the proportion of the cohort across transitions
    
    #### Undiscounted total expected LYs, QALYs and Costs ####
    ## LYs
    tot_ly_und   <- t(v_ly) %*% (v_wcc)
    ## QALYs
    tot_qaly_und <- t(v_qaly) %*% (v_wcc)
    ## Costs + Chemo
    tot_cost_und <- (t(v_cost) %*% (v_wcc)) + 
      ((c_Chemo + c_ChemoAdmin) * Trt) # Add Chemo cost WITHOUT WCC and discounting
    
    #### Discounted total expected LYs, QALYs and Costs ####
    ## LYs
    tot_ly   <- t(v_ly) %*% (v_dwe * v_wcc)
    ## QALYs
    tot_qaly <- t(v_qaly) %*% (v_dwe * v_wcc)
    ## Costs + Chemo
    tot_cost <- t(v_cost) %*% (v_dwc * v_wcc) + 
      ((c_Chemo + c_ChemoAdmin) * Trt) # Add Chemo cost WITHOUT WCC and discounting
    
    ### Store the results from the simulation in a list
    l_ce_out <- list(a_P = a_P,
                     m_M = m_M,
                     a_A = a_A,
                     # Undiscounted outcomes
                     tot_ly_und   = tot_ly_und,
                     tot_qaly_und = tot_qaly_und,
                     tot_cost_und = tot_cost_und,
                     # Discounted outcomes
                     tot_ly   = tot_ly,
                     tot_qaly = tot_qaly,
                     tot_cost = tot_cost
                     )
    ### Return the results
    return(l_ce_out)  
  }
  )
}

#' Calculate cost-effectiveness outcomes
#'
#' \code{calculate_ce_out} calculates costs and effects for a given vector of 
#' parameters using a decision model. This function needs to be modified by the 
#' users to fit their needs
#' @param l_params_all List with all parameters of decision model
#' @param n_wtp Willingness-to-pay threshold to compute net benefits.
#' @return 
#' A data frame with discounted costs, effectiveness and NMB for each strategy.
#' @export
calculate_ce_out <- function(l_params_all, 
                             n_wtp = 100000){ # User defined
  with(as.list(l_params_all), {
    #### Costs and QALYs for No Treatment ####
    l_out_notrt <- ce_model(l_params_all = l_params_all, 
                            p_CDX2neg_init = p_CDX2neg, 
                            Trt = FALSE)
    ## Store output
    cost_notrt <- l_out_notrt$tot_cost
    qaly_notrt <- l_out_notrt$tot_qaly
    ly_notrt   <- l_out_notrt$tot_ly
    m_M_notrt  <- l_out_notrt$m_M
    
    #### Costs and QALYs for Treat All ####
    l_out_trtall <- ce_model(l_params_all = l_params_all, 
                            p_CDX2neg_init = p_CDX2neg, 
                            Trt = TRUE)
    ## Store output
    cost_trtall <- l_out_trtall$tot_cost
    qaly_trtall <- l_out_trtall$tot_qaly
    ly_trtall   <- l_out_trtall$tot_ly
    m_M_trtall  <- l_out_notrt$m_M
    
    #### Costs and QALYs for Test and Treat ####
    ### Test characteristics
    sens_test <- 1
    spec_test <- 1
    
    ### Test outcomes
    ## Probability of testing positive
    p_test_pos <- p_CDX2neg * sens_test + (1 - p_CDX2neg) * (1 - spec_test) 
    ## Probability of testing negative
    p_test_neg <- 1 - p_test_pos
    ## Probability of Disease Positive GIVEN Testing Positive
    p_DisPos_TestPos <- p_CDX2neg*sens_test/p_test_pos
    ## Probability of Disease Negative GIVEN Testing Positive
    p_DisNeg_TestPos <- 1 - p_DisPos_TestPos
    ## Probability of Disease Positive GIVEN Testing Positive
    p_DisPos_TestNeg <- p_CDX2neg*(1 - sens_test)/(1 - p_test_pos)
    ## Probability of Disease Negative GIVEN Testing Positive
    p_DisNeg_TestNeg <- 1 - p_DisPos_TestNeg
    
    #### Run model for by CDX2 status and treatment status
    ### Run model with treatment
    l_cdx2neg_trt <- ce_model(l_params_all = l_params_all, 
                              p_CDX2neg_init = 1, Trt = TRUE)
    ### Run model without treatment
    l_cdx2neg_notrt <- ce_model(l_params_all = l_params_all, 
                                p_CDX2neg_init = 1, Trt = FALSE)
    ### Run model with treatment
    l_cdx2pos_trt <- ce_model(l_params_all = l_params_all, 
                              p_CDX2neg_init = 0, Trt = TRUE)
    ### Run model without treatment
    l_cdx2pos_notrt <- ce_model(l_params_all = l_params_all, 
                                p_CDX2neg_init = 0, Trt = FALSE)
    ## Extract Costs
    cost_cdx2neg_trt   <- l_cdx2neg_trt$tot_cost
    cost_cdx2neg_notrt <- l_cdx2neg_notrt$tot_cost
    cost_cdx2pos_trt   <- l_cdx2pos_trt$tot_cost
    cost_cdx2pos_notrt <- l_cdx2pos_notrt$tot_cost
    ## Extract QALYs
    qaly_cdx2neg_trt   <- l_cdx2neg_trt$tot_qaly
    qaly_cdx2neg_notrt <- l_cdx2neg_notrt$tot_qaly
    qaly_cdx2pos_trt   <- l_cdx2pos_trt$tot_qaly
    qaly_cdx2pos_notrt <- l_cdx2pos_notrt$tot_qaly
    ## Extract LYs
    ly_cdx2neg_trt   <- l_cdx2neg_trt$tot_ly
    ly_cdx2neg_notrt <- l_cdx2neg_notrt$tot_ly
    ly_cdx2pos_trt   <- l_cdx2pos_trt$tot_ly
    ly_cdx2pos_notrt <- l_cdx2pos_notrt$tot_ly
    ## Extract cohort trace
    m_M_cdx2neg_trt   <- l_cdx2neg_trt$m_M
    m_M_cdx2neg_notrt <- l_cdx2neg_notrt$m_M
    m_M_cdx2pos_trt   <- l_cdx2pos_trt$m_M
    m_M_cdx2pos_notrt <- l_cdx2pos_notrt$m_M
    ### Costs
    cost_test_n_treat <- (cost_cdx2neg_trt * p_DisPos_TestPos +
                         cost_cdx2pos_trt* p_DisNeg_TestPos) * p_test_pos +
      (cost_cdx2neg_notrt * p_DisPos_TestNeg +
         cost_cdx2pos_notrt * p_DisNeg_TestNeg) * p_test_neg +
      c_Test
    ### QALYs
    qaly_test_n_treat <- (qaly_cdx2neg_trt * p_DisPos_TestPos +
                      qaly_cdx2pos_trt * p_DisNeg_TestPos) * p_test_pos +
      (qaly_cdx2neg_notrt * p_DisPos_TestNeg +
         qaly_cdx2pos_notrt * p_DisNeg_TestNeg) * p_test_neg
    ### LYs
    ly_test_n_treat <- (ly_cdx2neg_trt * p_DisPos_TestPos +
                         ly_cdx2pos_trt * p_DisNeg_TestPos) * p_test_pos +
      (ly_cdx2neg_notrt * p_DisPos_TestNeg +
         ly_cdx2pos_notrt * p_DisNeg_TestNeg) * p_test_neg
    
    #### Vector with total discounted Costs and QALYs per strategy nd ICER ####
    v_tot_cost <- c(cost_notrt, cost_test_n_treat)
    v_tot_qaly <- c(qaly_notrt, qaly_test_n_treat)
    v_icer     <- c(NA, diff(v_tot_cost)/diff(v_tot_qaly))
    
    ## Vector with discounted net monetary benefits (NMB)
    v_nmb_d <- (v_tot_qaly * n_wtp) - v_tot_cost
    
    #### Data.frame with discounted costs, effectiveness and NMB #### 
    df_ce <- data.frame(Strategy = v_names_str,
                        Cost     = v_tot_cost,
                        Effect   = v_tot_qaly,
                        ICER     = v_icer,
                        NMB      = v_nmb_d)
    
    return(df_ce)
  }
  )
}

#' Generate lower and upper bound of the CEA parameters
#'
#' \code{generate_params_bounds} generates the lower and upper bounds of the
#' model parameters.
#' @param l_params_all List with all parameters of cost-effectiveness model.
#' @param seed Seed for reproducibility of Monte Carlo sampling.
#' @return 
#' A list with the lower and upper bounds of the model parameters.
#' @examples 
#' generate_params_bounds()
#' @export
generate_params_bounds <- function(l_params_all){
  with(as.list(l_params_all),{
    lb_factor <- 0.8
    ub_factor <- 1.2
    
    #--- Lower bounds ---#
    v_lb <- data.frame(
      ## Proportion of CDX2-negative patients 
      p_CDX2neg = 0.015, 
      # Proportion of recurrence being metastatic (CALIBRATED)
      p_Mets  = quantile(m_calib_post[, "p_Mets"], probs = 0.025), 
      # Cancer mortality rate (CALIBRATED)
      r_DieMets = quantile(m_calib_post[, "r_DieMets"], probs = 0.025),
      # Rate of recurrence in CDX2 positive patients (CALIBRATED)
      r_RecurCDX2pos = quantile(m_calib_post[, "r_RecurCDX2pos"], probs = 0.025), 
      # Hazard ratio of recurrence in CDX2 negative vs positive patients (CALIBRATED)
      hr_RecurCDX2neg = quantile(m_calib_post[, "hr_RecurCDX2neg"], probs = 0.025),
      # ## Hazard ratio for disease recurrence among patients with CDX2-negative 
      # # under chemo versus CDX2-negative patients without chemotherapy. From:
      # # André et al. JCO 2015 Table 1, Stage III DFS: 0.79 [0.67, 0.94]
      # hr_Recurr_CDXneg_Rx = 0.670,
      ## Hazard ratio for disease recurrence among patients with CDX2-negative
      # under chemo versus CDX2-negative patients without chemotherapy. From:
      # QUASAR. Lancet 2007 Figure 3, Stage II RR [99% CI]: 0.82 [0.63, 1.08]
      hr_Recurr_CDXneg_Rx = 0.63,
      # Hazard ratio for disease recurrence among patients with CDX2-positive 
      # under chemo versus CDX2-positive patients without chemotherapy. From: [TO BE ADDED]
      hr_Recurr_CDXpos_Rx = 0.95,
      
      ### State rewards
      ## Costs
      # Cost of chemotherapy
      c_Chemo = c_Chemo*lb_factor,
      # Cost of chemotherapy administration
      c_ChemoAdmin = c_ChemoAdmin*lb_factor, 
      # Initial costs in CRC Stage II (minus chemo and chemo admin) inflated from 2004 USD to 2018 USD using price index from PCE
      c_CRCStg2_init = c_CRCStg2_init - 1.96*339*inf_pce/n_cycles_year,
      # Continuing costs in CRC Stage II inflated from 2004 USD to 2018 USD using price index from PCE
      c_CRCStg2_cont =  c_CRCStg2_cont - 1.96*79*inf_pce/n_cycles_year, 
      # Continuing costs in CRC Stage IV inflated from 2004 USD to 2018 USD using price index from PCE
      c_CRCStg4_cont = c_CRCStg4_cont - 1.96*437*inf_pce/n_cycles_year,
      # Increase in cost when dying from cancer while in Stage II inflated from 2004 USD to 2018 USD using price index from PCE
      ic_DeathCRCStg2 = ic_DeathCRCStg2 - 1.96*705*inf_pce,
      # Increase in cost when dying from Other Causes (OC) while in Stage II inflated from 2004 USD to 2018 USD using price index from PCE
      ic_DeathOCStg2  = ic_DeathOCStg2 - 1.96*755*inf_pce,
      # Cost of IHC staining
      c_Test = 94,
      
      ## Utilities
      u_Stg2 = 0.69,      # Ness 1999, Outcome state "A" from table 3
      u_Stg2Chemo = 0.62, # Ness 1999, Outcome state "BC" from table 4
      u_Mets = 0.20       # Ness 1999, Outcome state "FG" from table 4
    ) 
    #--- Upper bounds ---#
    v_ub <- data.frame(
      ## Proportion of CDX2-negative patients 
      p_CDX2neg = 0.150, 
      # Proportion of recurrence being metastatic (CALIBRATED)
      p_Mets  = quantile(m_calib_post[, "p_Mets"], probs = 0.975), 
      # Cancer mortality rate (CALIBRATED)
      r_DieMets = quantile(m_calib_post[, "r_DieMets"], probs = 0.975),
      # Rate of recurrence in CDX2 positive patients (CALIBRATED)
      r_RecurCDX2pos = quantile(m_calib_post[, "r_RecurCDX2pos"], probs = 0.975), 
      # Hazard ratio of recurrence in CDX2 negative vs positive patients (CALIBRATED)
      hr_RecurCDX2neg = quantile(m_calib_post[, "hr_RecurCDX2neg"], probs = 0.975),
      # ## Hazard ratio for disease recurrence among patients with CDX2-negative 
      # # under chemo versus CDX2-negative patients without chemotherapy. From:
      # # André et al. JCO 2015 Table 1, Stage III DFS: 0.79 [0.67, 0.94]
      # hr_Recurr_CDXneg_Rx = 0.940,
      ## Hazard ratio for disease recurrence among patients with CDX2-negative
      # under chemo versus CDX2-negative patients without chemotherapy. From:
      # QUASAR. Lancet 2007 Figure 3, Stage II RR [99% CI]: 0.82 [0.63, 1.08]
      hr_Recurr_CDXneg_Rx = 1.080,
      # Hazard ratio for disease recurrence among patients with CDX2-positive 
      # under chemo versus CDX2-positive patients without chemotherapy. From: [TO BE ADDED]
      hr_Recurr_CDXpos_Rx = 1.000,
      
      ### State rewards
      ## Costs
      # Cost of chemotherapy
      c_Chemo = c_Chemo*ub_factor,
      # Cost of chemotherapy administration
      c_ChemoAdmin = c_ChemoAdmin*ub_factor, 
      # Initial costs in CRC Stage II (minus chemo and chemo admin) inflated from 2004 USD to 2018 USD using price index from PCE
      c_CRCStg2_init = c_CRCStg2_init + 1.96*339*inf_pce/n_cycles_year,
      # Continuing costs in CRC Stage II inflated from 2004 USD to 2018 USD using price index from PCE
      c_CRCStg2_cont = c_CRCStg2_cont + 1.96*79*inf_pce/n_cycles_year, 
      # Continuing costs in CRC Stage IV inflated from 2004 USD to 2018 USD using price index from PCE
      c_CRCStg4_cont = c_CRCStg4_cont + 1.96*437*inf_pce/n_cycles_year,
      # Increase in cost when dying from cancer while in Stage II inflated from 2004 USD to 2018 USD using price index from PCE
      ic_DeathCRCStg2 = ic_DeathCRCStg2 + 1.96*705*inf_pce,
      # Increase in cost when dying from Other Causes (OC) while in Stage II inflated from 2004 USD to 2018 USD using price index from PCE
      ic_DeathOCStg2  = ic_DeathOCStg2 + 1.96*755*inf_pce,
      # Cost of IHC staining
      c_Test = 179, # Cost of genetic test for CDX2neg
      
      ## Utilities
      u_Stg2 = 0.78,      # Ness 1999, Outcome state "A" from table 3
      u_Stg2Chemo = 0.72, # Ness 1999, Outcome state "BC" from table 4
      u_Mets = 0.31       # Ness 1999, Outcome state "FG" from table 3
    ) 
    #--- Standard errors based on bounds ---#
    v_se <- (v_ub - v_lb)/(2*1.96)
    
    #--- Return output ---#
    return(out = list(v_lb = v_lb,
                      v_ub = v_ub,
                      v_se = v_se))
  }
  )
}

#' Equal number of breaks
#'
#' This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes. Obtained from: 
#' https://stackoverflow.com/a/28459434
#' @param parms Vector with strings with the name of the parameters of interest
#' @param ranges A named list of the form c("parm" = c(0, 1), ...) that gives 
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of parameter values. If NULL, 100 parameter values are 
#' used
#' @param n_breaks Number of breaks
#' @param scale_factor Scaling factor
#' @param ... Further arguments to function (not used)
#' @return A function that generates equal number of breaks across facets
#' @export
equal_breaks <- function(n_breaks = 3, scale_factor = 0.05, ...){
  function(x){
    d <- scale_factor * diff(range(x)) / (1+2*scale_factor)
    seq(min(x)+d, max(x)-d, length = n_breaks)
  }
}

#' Adds aesthetics to all plots to reduce code duplication
#'
#' @param gplot a ggplot object
#' @param txtsize base text size
#' @param scale_name how to name scale. Default inherits from variable name.
#' @param col either none, full color, or black and white
#' @param col_aes which aesthetics to modify with \code{col}
#' @param lval color lightness - 0 to 100
#' @param greystart between 0 and 1. used in greyscale only. smaller numbers are lighter
#' @param greyend between 0 and 1, greater than greystart.
#' @param continuous which axes are continuous and should be modified by this function
#' @param n_x_ticks,n_y_ticks number of axis ticks
#' @param xbreaks,ybreaks vector of axis breaks.
#' will override \code{n_x_ticks} and/or \code{n_y_ticks} if provided.
#' @param facet_lab_txtsize text size for plot facet labels
#' @param xlim,ylim vector of axis limits, or NULL, which sets limits automatically
#' @param xtrans,ytrans transformations for the axes. See \code{\link[ggplot2]{scale_continuous}} for details.
#' @param xexpand,yexpand Padding around data. See \code{\link[ggplot2]{scale_continuous}} for details.
#' The default behavior in ggplot2 is \code{expansion(0.05)}. See \code{\link[ggplot2]{expansion}}
#' for how to modify this.
#' @param ... further arguments to plot.
#' This is not used by \code{dampack} but required for generic consistency.
#' @return a \code{ggplot2} plot updated with a common aesthetic
#'
#' @import ggplot2
#' @keywords internal
add_common_aes <- function(gplot, txtsize, scale_name = waiver(),
                           col = c("none", "full", "bw"),
                           col_aes = c("fill", "color"),
                           lval = 50,
                           greystart = 0.2,
                           greyend = 0.8,
                           continuous = c("none", "x", "y"),
                           n_x_ticks = 6,
                           n_y_ticks = 6,
                           xbreaks = NULL,
                           ybreaks = NULL,
                           xlim = NULL,
                           ylim = NULL,
                           xtrans = "identity",
                           ytrans = "identity",
                           xexpand = waiver(),
                           yexpand = waiver(),
                           facet_lab_txtsize = NULL,
                           ...) {
  p <- gplot +
    theme_bw() +
    theme(legend.title = element_text(size = txtsize),
          legend.text = element_text(size = txtsize - 3),
          title = element_text(face = "bold", size = (txtsize + 2)),
          axis.title.x = element_text(face = "bold", size = txtsize - 1),
          axis.title.y = element_text(face = "bold", size = txtsize - 1),
          axis.text.y = element_text(size = txtsize - 2),
          axis.text.x = element_text(size = txtsize - 2),
          strip.text.x = element_text(size = facet_lab_txtsize),
          strip.text.y = element_text(size = facet_lab_txtsize))
  
  col <- match.arg(col)
  col_aes <- match.arg(col_aes, several.ok = TRUE)
  if (col == "full") {
    p <- p +
      scale_color_discrete(name = scale_name, l = lval,
                           aesthetics = col_aes,
                           drop = FALSE)
  }
  if (col == "bw") {
    p <- p +
      scale_color_grey(name = scale_name, start = greystart, end = greyend,
                       aesthetics = col_aes,
                       drop = FALSE)
  }
  
  # axes and axis ticks
  continuous <- match.arg(continuous, several.ok = TRUE)
  
  if ("x" %in% continuous) {
    if (!is.null(xbreaks)) {
      xb <- xbreaks
    } else {
      xb <- number_ticks(n_x_ticks)
    }
    p <- p +
      scale_x_continuous(breaks = xb,
                         labels = labfun,
                         limits = xlim,
                         trans = xtrans,
                         expand = xexpand)
  }
  if ("y" %in% continuous) {
    if (!is.null(ybreaks)) {
      yb <- ybreaks
    } else {
      yb <- number_ticks(n_y_ticks)
    }
    p <- p +
      scale_y_continuous(breaks = yb,
                         labels = labfun,
                         limits = ylim,
                         trans = ytrans,
                         expand = yexpand)
  }
  return(p)
}

#' used to automatically label continuous scales
#' @keywords internal
#' @param x axis breaks
#' @return  a character vector giving a label for each input value
labfun <- function(x) {
  if (any(x > 999, na.rm = TRUE)) {
    comma(x)
  } else {
    x
  }
}

#' Number of ticks for \code{ggplot2} plots
#'
#' Function for determining number of ticks on axis of \code{ggplot2} plots.
#' @param n integer giving the desired number of ticks on axis of
#' \code{ggplot2} plots. Non-integer values are rounded down.
#' @section Details:
#' Based on function \code{pretty}.
#' @return a vector of axis-label breaks
#' @keywords internal
number_ticks <- function(n) {
  function(limits) {
    pretty(limits, n + 1)
  }
}