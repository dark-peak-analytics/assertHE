#' Decision model
#'
#' \code{decision_model} implements the decision model used.
#'
#' @param l_params_all List with all parameters of decision model
#' @param p_CDX2neg_init Initial proportion of CDX2-negative patients. Default
#' is NULL and will take the value of \code{p_CDX2neg} defined in
#' \code{load_all_params()}
#' @param Trt is this the Treat All strategy? (default is FALSE)
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default
#' = FALSE.
#' @param verbose Logical variable to indicate print out of messages. Default
#' = FALSE
#' @return
#' The transition probability array, the cohort trace matrix and the transition
#' dynamics array.
#' @export
decision_model <- function(l_params_all, p_CDX2neg_init = NULL, Trt = FALSE,
                           err_stop = FALSE, verbose = FALSE){ # User defined
  with(as.list(l_params_all), {
    #### Error checking ####
    if (n_cycles > length(v_r_mort_by_age_month)) {
      stop("Not all the ages in the age range have a corresponding mortality rate")
    }

    if (is.null(p_CDX2neg_init)){
      p_CDX2neg_init <- p_CDX2neg
    }
    ### Initial state vector
    v_s_init <- c(CDX2pos = 1 - p_CDX2neg_init,
                  CDX2neg = p_CDX2neg_init,
                  Mets    = 0,
                  Dead_OC = 0,
                  Dead_C  = 0)

    if ((sum(v_s_init) != 1) | !all(v_s_init >= 0)) {
      stop("vector of initial states (v_s_init) is not valid")
    }

    ### Recurrence parameters
    ## Rate of recurrence in CDX2 negative patients (adjusted by treatment status)
    r_RecurCDX2neg <- (r_RecurCDX2pos * hr_RecurCDX2neg) * (1 - Trt) + # If no treatment
      (r_RecurCDX2pos * hr_RecurCDX2neg * hr_Recurr_CDXneg_Rx) * (Trt) # Else, treatment
    ## Rate of recurrence in CDX2 positive patients (adjusted by treatment status)
    r_RecurCDX2pos_RxAdj <- r_RecurCDX2pos * (1 - Trt) + # If no treatment
      (r_RecurCDX2pos * hr_Recurr_CDXpos_Rx) * (Trt) # Else, treatment
    ## Probability of recurrence for CDX2 positive patients
    p_RecurCDX2pos <- 1 - exp(- r_RecurCDX2pos_RxAdj)
    ## Probability of recurrence for CDX2 negative patients
    p_RecurCDX2neg <- 1 - exp(- r_RecurCDX2neg)

    #### Age-specific transition probabilities ####
    # Age-specific background mortality
    v_p_DieAge  <- 1 - exp(-v_r_mort_by_age_month)
    ## Cancer mortality
    v_p_DieMets <- 1 - exp(- (v_r_mort_by_age_month + r_DieMets))

    #### Create age-specific transition probability matrices in an array ####
    ### Initialize array
    a_P <- array(NaN, dim = c(n_states, n_states, n_cycles),
                 dimnames = list(v_names_states, v_names_states, 0:(n_cycles-1)))
    ### Fill in array
    ## From CDX2 positive
    a_P["CDX2pos", "CDX2pos", ] <- (1 - v_p_DieAge) * (1 - p_RecurCDX2pos) +
      (1 - v_p_DieAge) * p_RecurCDX2pos * (1- p_Mets)
    a_P["CDX2pos", "CDX2neg", ] <- 0
    a_P["CDX2pos", "Mets", ]    <- (1 - v_p_DieAge) * p_RecurCDX2pos * p_Mets
    a_P["CDX2pos", "Dead_OC", ] <- v_p_DieAge
    a_P["CDX2pos", "Dead_C", ]  <- 0
    ## From CDX2 negative
    a_P["CDX2neg", "CDX2pos", ] <- 0
    a_P["CDX2neg", "CDX2neg", ] <- (1 - v_p_DieAge) * (1 - p_RecurCDX2neg) +
      (1 - v_p_DieAge) * p_RecurCDX2neg * ( 1- p_Mets)
    a_P["CDX2neg", "Mets", ]    <- (1 - v_p_DieAge) * p_RecurCDX2neg * p_Mets
    a_P["CDX2neg", "Dead_OC", ] <- v_p_DieAge
    a_P["CDX2neg", "Dead_C", ]  <- 0
    ## From Mets Recurrence
    a_P["Mets", "CDX2pos", ] <- 0
    a_P["Mets", "CDX2neg", ] <- 0
    a_P["Mets", "Mets", ]    <- (1 - v_p_DieMets)
    a_P["Mets", "Dead_OC", ] <- v_p_DieMets * v_r_mort_by_age_month/(v_r_mort_by_age_month + r_DieMets)
    a_P["Mets", "Dead_C", ]  <- v_p_DieMets * r_DieMets/(v_r_mort_by_age_month + r_DieMets)
    ## From Dead_OC
    a_P["Dead_OC", "CDX2pos", ] <- 0
    a_P["Dead_OC", "CDX2neg", ] <- 0
    a_P["Dead_OC", "Mets", ]    <- 0
    a_P["Dead_OC", "Dead_OC", ] <- 1
    a_P["Dead_OC", "Dead_C", ]  <- 0
    ## From Dead_C
    a_P["Dead_C", "CDX2pos", ] <- 0
    a_P["Dead_C", "CDX2neg", ] <- 0
    a_P["Dead_C", "Mets", ]    <- 0
    a_P["Dead_C", "Dead_OC", ] <- 0
    a_P["Dead_C", "Dead_C", ]  <- 1

    #### Check if transition array is valid ####
    ## Check that transition probabilities are [0, 1]
    darthtools::check_transition_probability(a_P, err_stop = err_stop, verbose = verbose)
    ## Check that all rows sum to 1
    darthtools::check_sum_of_transition_array(a_P, n_states = n_states, n_cycles = n_cycles, err_stop = err_stop, verbose = verbose)

    #### Compute cohort trace matrix and dynamic transition array for age-dependent STM ####
    # Initialize cohort trace matrix
    m_M <- matrix(0,
                  nrow = (n_cycles + 1), ncol = n_states,
                  dimnames = list(0:n_cycles, v_names_states))
    # Set first row of m.M with the initial state vector
    m_M[1, ] <- v_s_init

    # Initialize dynamic transition array
    a_A <- array(0,
                 dim      = c(n_states, n_states, n_cycles + 1),
                 dimnames = list(v_names_states, v_names_states, 0:n_cycles))
    # Set first slice of a_A with the initial state vector in its diagonal
    diag(a_A[, , 1]) <- v_s_init

    # Iterate STM over time
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      m_M[t + 1, ] <- m_M[t, ] %*% a_P[, , t]
      ## Fill in transition-dynamics array
      a_A[, , t + 1] <- m_M[t, ] * a_P[, , t]
    }
    return(list(a_P = a_P,
                m_M = m_M,
                a_A = a_A))
  }
  )
}

#----------------------------------------------------------------------------#
####                    Function to plot cohort trace                     ####
#----------------------------------------------------------------------------#
#' Plot cohort trace
#'
#' \code{plot_trace} plots the cohort trace.
#'
#' @param l_params_all List with all model parameters
#' @param m_M a cohort trace matrix
#' @return a ggplot object - plot of the cohort trace
#' @export
plot_trace <- function(l_params_all, m_M) {
  with(as.list(l_params_all), {
  df_M      <- data.frame(Cycle = 0:n_cycles, m_M, check.names = FALSE)
  df_M_long <- tidyr::gather(df_M, key = `Health State`, value, 2:ncol(df_M))
  df_M_long$`Health State` <- factor(df_M_long$`Health State`, levels = v_names_states)
  p <- ggplot(df_M_long, aes(x = Cycle, y = value,
                             color = `Health State`, linetype = `Health State`)) +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, 1), breaks = dampack::number_ticks(6)) +
    xlab("Cycle") +
    ylab("Proportion of the cohort") +
    theme_bw(base_size = 14) +
    theme(legend.position  = "bottom",
          legend.background = element_rect(fill = NA))

  return(p)
  }
  )
}
