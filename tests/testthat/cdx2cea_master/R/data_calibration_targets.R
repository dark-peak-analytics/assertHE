#' Calibration targets for the CDX2 STM model
#'
#' A list with calibration targets for the CDX2 STM
#' @format A data.frame with three calibration targets split in 2 rows and 9 
#' variables:
#' \itemize{\item Source: Source of outcome (i.e., calibration data)
#'          \item Outcome: Outcome name (i.e., disease-free survival (DFS), 
#'                overall survival (OS) and disease-specific survival (DSS)
#'          \item CDX2: CDX2 status (i.e., negative or positive)
#'          \item Time: Time in months
#'          \item S: Survival
#'          \item N: Sample size
#'          \item se: Standard error
#'          \item lb: Lower bound
#'          \item ub: Upper bound}
#' @md
"df_calibration_targets"