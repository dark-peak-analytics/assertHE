context("testing 02_simulation_model_functions.R")

library(dplyr)    # For data manipulation
library(cdx2cea)
library(darthtools)

# load inputs
l_params_all <- load_all_params()

#### Unit tests start ####
test_that("invalid inputs", {

  # We use an inaccurate input to raise a specific error message 
  # "Not all the age in the age range have a corresponding mortality rate" 
  l_params_all$n_cycles <- 421
  expect_error(decision_model(l_params_all), 
               "Not all the ages in the age range have a corresponding mortality rate")
  
  
  # We use an inaccurate input to raise a specific error message 
  # "vector of initial states (v_s_init) is not valid" 
  l_params_all$n_cycles <- 75
  l_params_all$p_CDX2neg <- -1
  expect_error(decision_model(l_params_all), 
               "vector of initial states \\(v_s_init\\) is not valid")
})

test_that("reproducing error message invalid transition probabiliies", {
  ## generate testing data
  for (i in 1:length(l_params_all)) {
    assign(names(l_params_all)[[i]], l_params_all[[i]])
  }
  
  ### Recurrence parameters
  ## Rate of recurrence in CDX2 negative patients (adjusted by treatment status)
  r_RecurCDX2neg <- (r_RecurCDX2pos * hr_RecurCDX2neg) # If no treatment
  ## Rate of recurrence in CDX2 positive patients (adjusted by treatment status)
  r_RecurCDX2pos_RxAdj <- r_RecurCDX2pos# If no treatment
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
  
  ## check if the correct input might produce unintended message
  expect_silent(check_transition_probability(a_P, err_stop = F, verbose = F))
  expect_silent(check_transition_probability(a_P, err_stop = F, verbose = T))
  expect_silent(check_transition_probability(a_P, err_stop = T, verbose = F))
  expect_silent(check_transition_probability(a_P, err_stop = T, verbose = T))
  
  ## check error messages of "check_transition_probability"
  a_P2 <- a_P
  # use an invalid value that would cause warning or error 
  a_P2["CDX2pos", "Mets", ] <- -0.03
  
  # we expect there is a warning message
  expect_warning(check_transition_probability(a_P2, err_stop = F, verbose = T))
  # we expect there is an error message
  expect_error(check_transition_probability(a_P2, err_stop = T, verbose = F))
  # we expect there is an error message instead of a warning message
  expect_error(check_transition_probability(a_P2, err_stop = T, verbose = T))
  
  ## check error messages of "check_sum_of_transition_array"
  # we expect there is an warning message
  expect_warning(check_sum_of_transition_array(a_P2, n_states, n_cycles, err_stop = F, verbose = T))  
  # we expect there is an error message
  expect_error(check_sum_of_transition_array(a_P2, n_states, n_cycles, err_stop = T, verbose = F))
  # we expect there is an error message instead of a warning message
  expect_error(check_sum_of_transition_array(a_P2, n_states, n_t, err_stop = T, verbose = T)) 
  
  ## testing whether the "check_" functions work properly in the decision_model function
  l_params_all2 <- l_params_all
  l_params_all2$r_DieMets <- -0.105
  
  expect_silent(decision_model(l_params_all2, err_stop = F, verbose = F))
  expect_error(decision_model(l_params_all2, err_stop = T, verbose = F))
})

test_that("correct outputs", {
  ## generate output data from decision_model
  output <- decision_model(l_params_all, err_stop = F, verbose = F)
  
  ## checking overall outputs
  # check the number of elements in the output
  expect_equal(length(output), 3)
  # check whether both the outputs are array type
  expect_true(all(unlist(lapply(output, is.array))))
  # check whether the output names are identical as expected 
  expect_identical(names(output), c("a_P", "m_M", "a_A"))
  
  ## checking output 1: a_P (the transition probability array)
  # check whether the dimension of a_P is as expected
  expect_equal(dim(output[[1]]), 
               c(l_params_all$n_states, l_params_all$n_states, l_params_all$n_cycles))
  # check whether all the transition probability is between 0 and 1
  expect_true(all(output[[1]] >= 0) | all(output[[1]] <= 1))
  # check whether all rows of each transition matrix sum up to 1 (sum_to_1)
  # because the sums are not numerically equal to 1, we made some adjustment (correct_small_digits)
  sum_to_1 <- apply(output[[1]], c(1, 3), function(x) sum(x)) 
  correct_small_digits <- round(sum_to_1 * 100) / 100
  expect_true(all(correct_small_digits == 1))
  
  ## checking output 2: m_M (trace matrix)
  # check whether the dimension of m_M is as expected
  expect_equal(dim(output[[2]]), 
               c(l_params_all$n_cycles + 1, l_params_all$n_states))
  # check whether each row in m_M sums up to 1 (sum_to_1)
  # because the sumes are not numerically equal to 1, we made some adjustment (correct_small_digits)
  sum_to_1 <- rowSums(output[[2]])
  correct_small_digits <- round(sum_to_1 * 100) / 100
  expect_true(all(correct_small_digits == 1))
  expect_true(all(output[[2]] >= 0 & output[[2]] <= 1))
})
