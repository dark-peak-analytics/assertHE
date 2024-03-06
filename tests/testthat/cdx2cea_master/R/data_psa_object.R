#' PSA object of the cost-effectiveness analysis decision model
#'
#' An object of class \code{psa} with PSA inputs and outputs for the Sick-Sicker model.
#' @format A list with 11 rows and 5 variables:
#' \describe{
#'   \item{n_strategies}{Number of strategies.}
#'   \item{strategies}{Strategy names.}
#'   \item{n_sim}{Number of PSA samples.}
#'   \item{cost}{A data frame with \code{n_sim} rows and \code{n_strategies} columns with the cost per strategy for all PSA samples.}
#'   \item{effectiveness}{A data frame with \code{n_sim} rows and \code{n_strategies} columns with the effectiveness per strategy for all PSA samples.}
#'   \item{parameters}{Survival target. A data frame of input parameters with 
#'   \code{n_sim} rows and 15 columns:
#'     \itemize{\item r_DieMets: Cancer mortality rate (CALIBRATED)
#'              \item r_RecurCDX2pos: Rate of recurrence in CDX2 positive patients (CALIBRATED)
#'              \item hr_RecurCDX2neg: Hazard ratio of recurrence in CDX2 negative vs positive patients (CALIBRATED)
#'              \item p_Mets: Proportion of recurrence being metastatic (CALIBRATED)
#'              \item p_CDX2neg: Proportion of CDX2-negative patients
#'              \item hr_Recurr_CDXneg_Rx: Hazard ratio for disease recurrence among patients with CDX2-negative under chemo versus CDX2-negative patients without chemotherapy.
#'              \item hr_Recurr_CDXpos_Rx: Hazard ratio for disease recurrence among patients with CDX2-positive under chemo versus CDX2-positive patients without chemotherapy.
#'              \item c_Chemo: Cost of chemotherapy
#'              \item c_ChemoAdmin: Cost of chemotherapy administration
#'              \item c_CRCStg2_init: Initial costs in CRC Stage II (minus chemo and chemo admin) 
#'              \item c_CRCStg2_cont: Continuing costs in CRC Stage II
#'              \item c_CRCStg4_cont: Continuing costs in CRC Stage IV
#'              \item ic_DeathCRCStg2: Increase in cost when dying from cancer while in Stage II
#'              \item ic_DeathOCStg2: Increase in cost when dying from Other Causes (OC) while in Stage II 
#'              \item c_Test: Cost of IHC staining
#'              \item u_Stg2: Utility of Stage II without chemo
#'              \item u_Stg2Chemo: Utility of Stage II with chemo
#'              \item u_Mets: Utility of metastatic recurrence state}}
#'   \item{parnames}{A vector of strings with parameter names.}
#'   \item{currency}{A string with the currency.}
#' }
"l_psa"