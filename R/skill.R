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


#' @title True Skill Statistic (TSS) Calculator
#' @description Calculates the True Skill Statistic for binary classification in stock assessments.
#' @param TP True Positives (correct overfished predictions)
#' @param TN True Negatives (correct healthy stock predictions)
#' @param FP False Positives (false overfished predictions)
#' @param FN False Negatives (false healthy stock predictions)
#' @return Numeric TSS value ranging from -1 to +1 (perfect skill)
#' @examples
#' TSS(TP=45, TN=30, FP=10, FN=15) # Good skill (0.5)
#' @export
TSS <- function(TP,TN,FP,FN) TP/(FN+TP) - TN/(FP+TN)

  
#' @title Receiver Operating Characteristic (ROC) Curve Generator
#' @description Creates ROC curve data for stock status classification performance.
#' @param labels Logical vector of true stock statuses (TRUE=overfished)
#' @param scores Numeric vector of model predictions (e.g., B/BMSY ratios)
#' @return Data.frame with columns:
#' \itemize{
#'   \item TPR: True Positive Rate (sensitivity)
#'   \item FPR: False Positive Rate (1 - specificity)
#'   \item labels: Ordered true labels
#'   \item reference: Threshold values
#' }
#' @examples
#' obs <- runif(100, 0.5, 1.5) < 1
#' pred <- obs + rnorm(100, sd=0.3)
#' roc_data <- rocFn(obs, pred)
#' @export
rocFn <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR = cumsum(labels)/sum(labels),
             FPR = cumsum(!labels)/sum(!labels),
             labels,
             reference = sort(scores))}

#' @title Confusion Matrix Calculator
#' @description Computes confusion matrix components for stock status classification.
#' @param x Numeric vector of observed values (e.g., true B/BMSY)
#' @param y Numeric vector of predicted values
#' @return Data.frame with:
#' \itemize{
#'   \item TP: True positives (both < 1)
#'   \item TN: True negatives (both >= 1)
#'   \item FP: False positives (observed <1 but predicted >=1)
#'   \item FN: False negatives (observed >=1 but predicted <1)
#' }
#' @examples
#' obs <- runif(100, 0.5, 1.5)
#' pred <- obs * exp(rnorm(100, sd=0.2))
#' PN(obs < 1, pred < 1)
#' @export
PN <- function(x,y) {
  data.frame(TP = sum(x >=0 & y >=0),
             TN = sum(x < 0 & y < 0),
             FP = sum(x >=0 & y < 0),
             FN = sum(x < 0 & y >=0))}

#' @title Stock Assessment Skill Visualizer
#' @description Generates diagnostic plots for management procedure evaluation.
#' @param x Data.frame containing assessment results
#' @param state Character name of column with true stock status
#' @param indicator Character name of column with predicted status
#' @param xLabel Axis label for plots (default="")
#' @param limits X-axis limits for density plots (default=c(0,5))
#' @return ggplot object with 3-panel visualization:
#' \itemize{
#'   \item a) Density plots of observed vs predicted status
#'   \item b) Scatterplot with reference lines
#'   \item c) ROC curve with AUC
#' }
#' @examples
#' \dontrun{
#' data(assessment_data)
#' plotSkill(assessment_data, "true_status", "predicted_status")
#' }
#' @import ggplot2
#' @export
plotSkill <- function(x, state, indicator, xLabel="", limits=c(0,5)) {
  
    dat=transform(x,state  =eval(sym(state)),
                  indicator=eval(sym(indicator)))
    
    dat=subset(   dat, !is.na(state)&!is.na(indicator))
    dat=transform(dat, ratio=(indicator-state)/state)
    smry=ddply(dat, .(Scenario), with, {
      om=state
      mp=indicator
      roc1=rocFn(om>1,mp)
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
      rtn})
    
    rocDat=ddply(dat, .(Scenario), with, tryIt(rocFn(state>1,indicator)))
    
    p1=ggplot()+
      geom_density(data=dat,aes(x=state),    fill="green",   alpha=0.5)+
      geom_density(data=dat,aes(x=indicator),fill="#E69F00", alpha=0.5)+
      geom_density(data=dat,aes(x=indicator),fill="red",     alpha=0.5)+
      facet_grid(Scenario ~ ., scales = "free")+
      geom_vline(xintercept = 1, color = "red")+
      geom_label(data=smry,
                 aes(label=round(TSS, 2)),
                 x = Inf, y =-Inf,
                 hjust = 1, vjust = 1,
                 size  = 4, alpha=0.6)+
      scale_y_continuous(expand=c(0, 0))+
      scale_x_continuous(limits=limits)+
      labs(title="a)",x=xLabel)
    
    # Bias plot
    p2=ggplot(dat)+
      geom_boxplot(aes(ratio), fill="#E69F00", alpha=0.5)+
      coord_flip()+
      geom_vline(xintercept=0, col="red")+
      facet_grid(Scenario~.)+
      coord_cartesian(xlim=c(-1,5))+
      labs(title="b)", x=xLabel)
    
    # Predictions plot
    p3=ggplot(dat[sample(seq(dim(dat)[1]),pmin(dim(dat)[1],1000)),])+
      facet_grid(Scenario~.)+
      geom_point(aes(state,indicator), size=2.5)+
      geom_vline(xintercept=1)+
      geom_hline(aes(yintercept=1), col="blue")+
      geom_hline(aes(yintercept=ref),data=smry,col="red")+
      scale_x_continuous(limits=limits)+
      scale_y_continuous(limits=limits)+
      labs(title="b)", 
           x="ICES Cat1", 
           y="JABBA")+
      geom_label(aes(x=Inf, y=ref,
                     hjust=1, vjust=0,
                     label=paste("BSS=",round(BSS,2))), size=4.0, col="red",
                 data=smry)+
      geom_label(aes(x=limits[1], y=1,
                     hjust=0, vjust=0, 
                     label=paste("TSS=",round(TSS,2))), size=4.0, col="blue",
                 data=smry)+
      theme(legend.position="none")+
      scale_x_log10()+scale_y_log10()
    
    p4=ggplot(rocDat)+
      facet_grid(Scenario~.)+
      geom_path( aes(FPR, TPR), alpha=0.5)+
      geom_point(aes(FPR, TPR), data=smry,col="blue",size=2)+
      geom_point(aes(FPR2,TPR2),data=smry,col="red", size=2)+
      geom_abline(intercept=0, slope=1, linetype=2, linewidth=1.0, col="grey74")+
      geom_label(data=smry,
                 aes(label=paste("AUC=",round(AUC, 2))),
                 x    =1.0, y    =0.2,
                 hjust=1,   vjust=1,
                 size =4)+
      labs(title="c)",
           x="FPR (1-Specificity)",
           y="TPR (Sensitivity)")
    
    # AUC histogram
    p5=ggplot(rocDat)+
      geom_histogram(aes(AUC), bins=30)+
      facet_grid(Scenario~.)+
      geom_vline(xintercept=0.5, linetype=2, size=1.0, col="red")+
      labs(title="AUC")+
      theme_minimal()
    
    # Modify your plots
    p1=p1+FLCandy:::theme_no_y+FLCandy:::theme_no_title
    p2=p2+FLCandy:::theme_no_y+FLCandy:::theme_no_title
    p3=p3+FLCandy:::theme_no_y+FLCandy:::theme_no_title
    p4=p4+FLCandy:::theme_no_y
    #p5=p5+FLCandy:::theme_auc
    
    # Combine plots with minimal spacing
    combined=ggarrange(p1,p3,p4,
                       ncol  = 3, 
                       widths=c(1.25, 1, 1),
                       common.legend = TRUE,
                       legend = "none",
                       align = "h")
    
    combined=annotate_figure(combined,
                             top=text_grob("Assessment Performance Metrics", 
                                           face="bold", size=14))
    
    return(combined)}  
  
#' @title Time Series Comparison Metrics
#' @description Calculates similarity measures between two stock assessment time series.
#' @param ts1 Numeric vector (e.g., observed biomass index)
#' @param ts2 Numeric vector (e.g., model-predicted biomass)
#' @return Data.frame with:
#' \itemize{
#'   \item rmse: Root Mean Square Error
#'   \item correlation: Pearson correlation
#'   \item sd: Standard deviation of residuals
#' }
#' @examples
#' obs <- cumsum(rnorm(20))
#' pred <- obs + rnorm(20, sd=0.5)
#' compareTS(obs, pred)
#' @export
compareTS <- function(ts1, ts2) {

    # Ensure same length
    min_length=min(length(ts1), length(ts2))
    ts1=ts1[1:min_length]
    ts2=ts2[1:min_length]
    
    # Basic statistics
    rmse=sqrt(mean((ts1 - ts2)^2))
    correlation=cor(ts1, ts2)
    
    # Return results as list
    results=data.frame(rmse       =rmse,
                       correlation=correlation,
                       sd         =var(ts2-ts1)^0.5)
    
    return(results)}

#' @title Optimal Lag Finder
#' @description Identifies lag with maximum cross-correlation between stock assessment time series.
#' @param obs Observed time series (e.g., survey index)
#' @param hat Predicted time series (e.g., model output)
#' @param lag.max Maximum lag to consider (default=5)
#' @return Data.frame with optimal lag and corresponding ACF value
#' @examples
#' obs <- sin(seq(0, 2*pi, length=50)) + rnorm(50)
#' hat <- lag(obs, 2) + rnorm(50, sd=0.2)
#' ccfFn(obs, hat)
#' @export
ccfFn <- function(obs, hat, lag.max=5){
    rtn=ccf(obs,hat,plot=FALSE,lag.max=lag.max)
    subset(data.frame(lag=rtn$lag,
                      acf=rtn$acf),acf==max(acf))}
  
  
#' @title Taylor Diagram Generator
#' @description Creates Taylor diagrams for visual model skill assessment in stock assessments.
#' @param min_R Minimum reference value (default=0.25)
#' @param max_R Maximum reference value (default=1.75)
#' @param contours Number of contour lines (default=7)
#' @param n_lines Number of angular lines (default=10)
#' @param x_0 Central reference point (default=1)
#' @param ref_r_min Minimum reference circle radius (default=0.25)
#' @param ref_r_max Maximum reference circle radius (default=2)
#' @param ref_contours Number of reference circles (default=8)
#' @param full Display full circle (TRUE) or quadrant (FALSE) (default=FALSE)
#' @return ggplot object showing:
#' \itemize{
#'   \item Standard deviation ratios
#'   \item Correlation coefficients
#'   \item RMS differences
#' }
#' @examples
#' taylorDiagram()
#' @import ggplot2
#' @export
taylorDiagram<-function(min_R=0.25, max_R=1.75, contours=7, 
                              n_lines=10, x_0=1, ref_r_min=0.25,
                              ref_r_max=2, ref_contours=8, full=FALSE) {
    # Create base plot structure
    p=ggplot()+
      theme_minimal()+
      coord_equal()+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(expand = c(0, 0))+
      labs(x = "Standard Deviation", y = "σ")
    
    # Add correlation contours
    angles=seq(0, ifelse(full, pi, pi/2), length.out = n_lines)
    radii=seq(min_R, max_R, length.out = contours)
    
    # Add reference circles
    for(r in radii) {
      circle_data=data.frame(
        x = r * cos(angles),
        y = r * sin(angles)
      )
      p=p + geom_path(data = circle_data, aes(x, y), 
                      linetype = "dashed", color = "gray70")
    }
    
    return(p)}

