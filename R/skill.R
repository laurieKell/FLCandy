#' @title Calculate Prediction Skill Scores
#' @description Computes threshold-based skill scores including TSS (True Skill Statistic) and AUC (Area Under the Curve) for fishery stock assessment models.
#' @param x Numeric vector of model predictions (e.g., estimated stock biomass)
#' @param y Numeric vector of observed values (e.g., survey biomass index)
#' @param threshold Optional numeric vector of decision thresholds for classification
#' @param reference Reference value for stock status classification (default = 1, typical BMSY threshold)
#' @return Data.frame containing optimal threshold and associated metrics:
#' \itemize{
#'   \item ref: Optimal reference threshold
#'   \item TP: True positives at threshold
#'   \item TN: True negatives at threshold
#'   \item FP: False positives at threshold
#'   \item FN: False negatives at threshold  
#'   \item TSS: True Skill Statistic (TPR - FPR)
#'   \item TPR: True Positive Rate (sensitivity)
#'   \item FPR: False Positive Rate (1 - specificity)
#'   \item AUC: Area Under ROC Curve
#' }
#' @examples
#' # Simulate stock assessment data
#' obs_status <- rlnorm(100, meanlog = log(1), sdlog = 0.5)
#' pred_status <- obs_status * exp(rnorm(100, sd = 0.3))
#' skillScore(pred_status, obs_status > 1, reference = 1)
#' @importFrom FLCore auc
#' @export
skillScore <- function(x, y, threshold=NULL, reference=1) {
    if (is.null(threshold))
      threshold=sort(unique(y)) # Use unique thresholds from y (mp)
    
    auc<-function(TPR, FPR) {
      dFPR =c(diff(FPR), 0)
      dTPR =c(diff(TPR), 0)
      sum(TPR*dFPR)+sum(dTPR*dFPR)/2}
    
    results=sapply(threshold, function(thresh) {
      TP=sum(x >=reference & y>=thresh)
      TN=sum(x < reference & y< thresh)
      FP=sum(x < reference & y>=thresh)
      FN=sum(x >=reference & y< thresh)
      
      c(TP =TP,
        TN =TN,
        FP =FP,
        FN =FN,
        TSS=TP/(TP+FN)-FP/(FP+TN),
        TPR=TP/(TP+FN),
        FPR=FP/(FP+TN))})
    
    # Find index of maximum TSS
    if (all(is.na(results["TSS",]))) return(NULL)
    
    maxIdx=try(which.max(results["TSS", ]))
    
    AUC=with(rocFn(x>reference,y),auc(TPR,FPR))
    
    # Return results at optimal threshold
    data.frame(
      ref=threshold[     maxIdx],
      TP =results["TP",  maxIdx],
      TN =results["TN",  maxIdx],
      FP =results["FP",  maxIdx],
      FN =results["FN",  maxIdx],
      TSS=results["TSS", maxIdx],
      TPR=results["TPR", maxIdx],
      FPR=results["FPR", maxIdx],
      AUC=AUC)}

#' @title Stock Assessment Model Summary
#' @description Computes comprehensive performance metrics for fishery management procedures.
#' @param om Numeric vector of operating model outputs (true values)
#' @param mp Numeric vector of management procedure predictions
#' @return Data.frame containing:
#' \itemize{
#'   \item AUC: Area Under ROC Curve
#'   \item TSS: True Skill Statistic at reference threshold
#'   \item BSS: Best achievable skill score
#'   \item ref: Optimal reference threshold
#'   \item TPR/FPR: Rates at reference threshold
#'   \item TPR2/FPR2: Rates at optimal threshold
#' }
#' @examples
#' om <- runif(100, 0.5, 1.5)
#' mp <- om * exp(rnorm(100, sd=0.2))
#' skillSummary(om, mp)
#' @export
skillSummary<-function(om,mp){
    
    roc1=rocFn(om,mp)
    AUC =with(roc1,FLCore:::auc(TPR=TPR,FPR=FPR))
    TPR =roc1$TPR
    FPR =roc1$FPR
    ref =roc1$reference
    flag=min((ref-1)^2)==(ref-1)^2
    flg2=(TPR-FPR)==max(TPR-FPR)
    
    rtn =data.frame(AUC=AUC,
                    TSS=((TPR-FPR)[flag])[1],
                    BSS=((TPR-FPR)[flg2])[1],
                    ref=ref[flg2][1],
                    TPR=TPR[flag][1],
                    FPR=FPR[flag][1],
                    TPR2=TPR[flg2][1],
                    FPR2=FPR[flg2][1])
    rtn}

#' @title Trend Agreement Metrics
#' @description Calculates correlation measures between observed and predicted stock trends.
#' @param obs Numeric vector of observed time series (e.g., biomass index)
#' @param hat Numeric vector of predicted time series
#' @return Data.frame containing:
#' \itemize{
#'   \item pearson: Pearson correlation coefficient
#'   \item spearman: Spearman's rank correlation  
#'   \item direction: Proportion of matching inter-annual change directions
#' }
#' @examples
#' obs_trend <- cumsum(rnorm(20))
#' pred_trend <- obs_trend + rnorm(20, sd=0.5)
#' trend(obs_trend, pred_trend)
#' @export
trend<-function(obs,hat){ 
    data.frame(
      pearson  =cor(hat, obs, method = "pearson"),
      spearman =cor(hat, obs, method = "spearman"),
      direction=mean(sign(diff(hat)) == sign(diff(obs)), na.rm = TRUE))}


#' @title Stock Status Classification Metrics
#' @description Evaluates performance of stock status classification (Overfished/Healthy).
#' @param obs Numeric vector of observed stock status values
#' @param hat Numeric vector of predicted status values
#' @return Data.frame containing:
#' \itemize{
#'   \item accuracy: Overall classification accuracy
#'   \item precision: Precision for Overfished classification
#'   \item recall: Recall for Overfished classification
#' }
#' @note Status determined using threshold at 1 (B/BMSY). Check variable names in function code.
#' @examples
#' obs_status <- runif(100, 0.5, 1.5)
#' pred_status <- obs_status * exp(rnorm(100, sd=0.2))
#' state(obs_status, pred_status)
#' @export
state<-function(obs,hat){
  
  obs =ifelse(obs < 1, "Overfished", "Healthy")
  pred=ifelse(hat < 1, "Overfished", "Healthy")
  
  data.frame(
    accuracy  =mean(obs_status==pred_status),
    precision =sum( obs =="Overfished" & pred=="Overfished")/sum(pred=="Overfished"),
    recall    =sum( obs =="Overfished" & pred=="Overfished")/sum(obs =="Overfished"))}


#' @title Variability Comparison Metrics
#' @description Compares variability characteristics between observed and predicted time series.
#' @param obs Numeric vector of observed values
#' @param hat Numeric vector of predicted values  
#' @return Data.frame containing variability ratios:
#' \itemize{
#'   \item sd: Standard deviation ratio (pred/obs)
#'   \item iqr: Interquartile range ratio
#'   \item cv: Coefficient of variation ratio
#' }
#' @examples 
#' obs <- rlnorm(100, meanlog = log(1), sdlog = 0.4)
#' hat <- obs * exp(rnorm(100, sd=0.1))
#' variability(obs, hat)
#' @export
variability<-function(obs,hat){
  data.frame(sd =sd(hat)/sd(obs),
             iqr=IQR(hat)/IQR(obs),
             cv =(sd(hat)/mean(hat))/(sd(obs)/mean(obs)))}


#' @title Comprehensive Diagnostic Evaluation
#' @description Integrates multiple performance metrics for stock assessment model validation.
#' @param obs Numeric vector of observed values
#' @param hat Numeric vector of predicted values
#' @param ndemb Embedding dimension for permutation entropy calculation (default=5)
#' @return Data.frame containing 10 diagnostic metrics. Final classification currently non-functional.
#' @note Requires helper functions: stdz(), permutation_entropy(), ordinal_pattern_distribution()
#' @examples
#' obs <- runif(100, 0.8, 1.2)
#' hat <- obs * exp(rnorm(100, sd=0.15))
#' diagnostics(obs, hat)
#' @export
diagnostics<-function(obs,hat,ndemb=5){
  rocFn<-function(labels, scores){
    labels=labels[order(scores, decreasing=TRUE)]
    data.frame(TPR=cumsum(labels)/sum(labels),
               FPR=cumsum(!labels)/sum(!labels),
               labels,
               reference=sort(scores))}
  
  roc=rocFn(stdz(obs)>1,stdz(hat))
  tss=skillScore(stdz(hat),stdz(obs)-1)
  
  return(
    data.frame(
      trend      =cor(hat, obs),
      status     =mean((hat<1) == (obs<1)),
      sd.hat     =sd(hat),
      sd.obs     =sd(obs),
      variability=sd(hat)/sd(obs),
      auc        =FLCore:::auc(TPR=roc$TPR,FPR=roc$FPR),
      tss        =tss$TSS,
      fpr        =tss$FPR,
      tpr        =tss$TPR,
      entropy    =permutation_entropy(ordinal_pattern_distribution(obs, ndemb=ndemb))))
  
  case_when(
    trend > 0.7 & status > 0.8 & between(variability,0.8,1.2) ~ "Excellent",
    trend > 0.5 & status > 0.7 & between(variability,0.6,1.4) ~ "Adequate",
    TRUE ~ "Needs Improvement")}
