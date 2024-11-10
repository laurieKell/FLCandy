#' Calculate Receiver Operating Characteristic (ROC) Metrics
#' 
#' @description
#' Computes various performance metrics including True Skill Statistic (TSS),
#' True Positive Rate (TPR), and True Negative Rate (TNR) for binary classification
#' based on Fielding & Bell, 1997.
#'
#' @param label Numeric matrix or vector containing binary values (0,1) representing
#'        true observations
#' @param ind Numeric matrix or vector of same dimensions as label containing
#'        indicator or prediction values
#'
#' @return A data frame containing the following columns:
#'   \item{label}{Original binary labels}
#'   \item{TP}{True Positives}
#'   \item{TN}{True Negatives}
#'   \item{FP}{False Positives}
#'   \item{FN}{False Negatives}
#'   \item{TPR}{True Positive Rate (Sensitivity)}
#'   \item{FPR}{False Positive Rate}
#'   \item{TSS}{True Skill Statistic}
#'
#' @details
#' The True Skill Statistic is calculated as: TSS = TPR + TNR - 1
#' where:
#' - TPR (sensitivity) = TP / (TP + FN)
#' - TNR (specificity) = TN / (TN + FP)
#'
#' @examples
#' data(ple4)
#' # OM 'realities' on stock status relative to refpt
#' label <- rlnorm(100, log(ssb(ple4))[, '2017'], 0.2) < 850000
#' # Model estimates of SSB
#' ind <- rlnorm(100, log(ssb(ple4) * 1.05)[,'2017'], 0.6)
#' # Compute TSS, returns data.frame
#' roc(label, ind)
#'
#' @references
#' Fielding, A.H. and Bell, J.F. (1997) A review of methods for the assessment of
#' prediction errors in conservation presence/absence models.
#' Environmental Conservation, 24(1), 38-49.
#'
#' @export
#' @importFrom stats model.frame
#' @importFrom FLCore FLQuants

