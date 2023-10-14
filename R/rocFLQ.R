# methods.R - DESC
# /home/mosqu003/Active/METHODS_prediction_error/methods.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# True Skill Statistic
# Fielding & Bell, 1997.

# TSS = TPR + TNR - 1

# True Positive Rate, sensitivity
# TPR = TP / (TP + FN)

# True Negative Rate, specificity
# TNR = TN / (TN + FP)

# Confusion matrix
#                 Prediction
# Observation     1     0      sum
# 1               TP    FN
# 0               FP    TN
# sum


require(FLCore)

#' @examples
#' data(ple4)
#' # OM 'realities' on stock status relative to refpt
#' label <- rlnorm(100, log(ssb(ple4))[, '2017'], 0.2) < 850000
#' # Model estimates of SSB
#' ind <- rlnorm(100, log(ssb(ple4) * 1.05)[,'2017'], 0.6)
#' # Compute TSS, returns data.frame
#' roc(label, ind)
#' ggplot(roc(label, ind), aes(x=TPR, y=FPR)) +
#'   geom_line() +
#'   geom_abline(slope=1, intercept=0, colour="red", linetype=2)
#' # Multiple years are computed together, but labelled
#' label <- rlnorm(100, log(ssb(ple4)[, 52:61]), 0.2) < 850000
#' ind <- rlnorm(100, log(ssb(ple4)[, 52:61] * 1.05), 0.6)
#' roc(label, ind)
#' # BUG: BUT years do not work
#' ggplot(roc(label, ind), aes(x=TPR, y=FPR)) +
#'   geom_line()

roc <- function(label, ind) {

  # CHECK dims match
  if(!all.equal(dim(label), dim(ind)))
    stop("dimensions of label and ind must match")

  # CHECK label is pseudo-logical (0, 1)
  if(!all(c(label) %in% c(0, 1)))
    stop("label can only contain 0 and 1 for FALSE, TRUE")
  
  # ORDER by descending ind
  idx <- rev(order(c(ind)))
  label <- c(label)[idx]
 
  # CALCULATE TRUE and FALSE positives
  tp <- cumsum(label)
  fp <- cumsum(!label)
  
  # CALCULATE TRUE and FALSE negatives
  tn <- sum(!label) - fp  
  fn <- sum(label) - tp  

  # CALCULATE TRUE and FALSE positive rates
  tpr <- tp / (tp + fn)
  tnr <- tn / (tn + fp)

  # COMPUTE True Skill ind
  tss <- tpr + tnr - 1
 
  # CONSTRUCT output data.frame
  out <- model.frame(FLQuants(ind=ind), drop=TRUE)[idx,]

  res <- cbind(out, label=label, TP=tp, TN=tn,
    FP=fp, FN=fn, TPR=tpr, FPR=fpr, TSS=tss)

  return(res)
}



roc <- function(state, score) {

  # CHECK dims match
  if(!all.equal(dim(state), dim(score)))
    stop("dimensions of state and score must match")

  # CHECK state is pseudo-logical (0, 1)
  if(!all(c(state) %in% c(0, 1)))
    label <- state > 1
  else
    label <- state == 1
  
  # ORDER by descending score
  idx <- rev(order(c(score)))
  label <- c(label)[idx]
 
  # CALCULATE TRUE and FALSE positives
  tp <- cumsum(label)
  fp <- cumsum(!label)
  
  # CALCULATE TRUE and FALSE negatives
  tn <- sum(!label) - fp  
  fn <- sum(label) - tp  

  # CALCULATE TRUE and FALSE positive rates
  tpr <- tp / (tp + fn)
  tnr <- tn / (tn + fp)

  # COMPUTE True Skill Score
  tss <- tpr + tnr - 1
 
  # CONSTRUCT output data.frame
  out <- model.frame(FLQuants(score=score), drop=TRUE)[idx,]

  res <- cbind(out, label=label, state=c(state)[idx], TP=tp, TN=tn,
    FP=fp, FN=fn, TPR=tpr, FPR=fpr, TSS=tss)

  return(res)
}

