# Create background rectangles using geom_rect
tssCat=data.frame(
  xmin=rep(0,5),
  xmax=rep(3,5),
  ymin=c(-Inf, 0.0, 0.2, 0.4, 0.6),
  ymax=c( 0.2, 0.2, 0.4, 0.6, Inf),
  quality=factor(       c("Negative","None","Low","Moderate","High"),
                 levels=c("Negative","None","Low","Moderate","High")))

tssCol=c("Negative"="#ffaaaa",
         "None"    ="#ffcccc", "Low" ="#ffe6cc", 
         "Moderate"="#e6ffcc", "High"="#ccffe6")

aucCat=data.frame( 
  xmin=rep(-Inf, 4),
  xmax=rep( Inf, 4),
  ymin=c(-Inf,0.6, 0.7, 0.9),
  ymax=c( 0.6,0.7, 0.9, Inf),
  quality=factor(       c("Fail","Fair","Good","Excellent"),
                 levels=c("Fail","Fair","Good","Excellent")))

aucCol=c("Fail"="#ffcccc", "Fair"     ="#ffe6cc", 
         "Good"="#e6ffcc", "Excellent"="#ccffe6")

fnTss<-function(x) {
  chk = unique(c(tssCat$ymin, tssCat$ymax))
  sapply(x, function(val) {
    tssCol[max(seq(length(chk))[val > chk])]
  })}

fnAuc<-function(x) {
  chk = unique(c(aucCat$ymin, aucCat$ymax))
  sapply(x, function(val) {
    aucCol[max(seq(length(chk))[val > chk])]
  })}

aucTss<-function(om,mp){
  rtn=FLCandy:::roc2(om,mp) 
  
  data.frame("tss"=data.frame(with(rtn[abs(rtn$reference-1)==min(abs(rtn$reference-1)),],TPR-FPR))[1,],
             data.frame("auc"=pROC:::auc(as.character(rtn$label),rtn$reference)))}

#dat=data.frame(om=rep(c(1,2),10),mp=rlnorm(20))

#aucTss(dat$om>1,dat$mp)


auc<-function(TPR, FPR) {
  dFPR=c(diff(FPR), 0)
  dTPR=c(diff(TPR), 0)
  sum(TPR*dFPR)+sum(dTPR*dFPR)/2}

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
#' obs_trend=cumsum(rnorm(20))
#' pred_trend=obs_trend + rnorm(20, sd=0.5)
#' trend(obs_trend, pred_trend)
#' @export
trend<-function(obs,hat){ 
    data.frame(
      pearson =cor(hat, obs, method="pearson"),
      spearman=cor(hat, obs, method="spearman"),
      direction=mean(sign(diff(hat))== sign(diff(obs)), na.rm=TRUE))}


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
#' obs_status=runif(100, 0.5, 1.5)
#' pred_status=obs_status * exp(rnorm(100, sd=0.2))
#' state(obs_status, pred_status)
#' @export
state<-function(obs,hat){
  
  obs=ifelse(obs < 1, "Overfished", "Healthy")
  pred=ifelse(hat < 1, "Overfished", "Healthy")
  
  data.frame(
    accuracy =mean(obs_status==pred_status),
    precision=sum( obs=="Overfished" & pred=="Overfished")/sum(pred=="Overfished"),
    recall   =sum( obs=="Overfished" & pred=="Overfished")/sum(obs=="Overfished"))}


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
#' obs=rlnorm(100, meanlog=log(1), sdlog=0.4)
#' hat=obs * exp(rnorm(100, sd=0.1))
#' variability(obs, hat)
#' @export
variability<-function(obs,hat){
  data.frame(sd=sd(hat)/sd(obs),
             iqr=IQR(hat)/IQR(obs),
             cv=(sd(hat)/mean(hat))/(sd(obs)/mean(obs)))}


#' @title Comprehensive Diagnostic Evaluation
#' @description Integrates multiple performance metrics for stock assessment model validation.
#' @param obs Numeric vector of observed values
#' @param hat Numeric vector of predicted values
#' @param ndemb Embedding dimension for permutation entropy calculation (default=5)
#' @return Data.frame containing 10 diagnostic metrics. Final classification currently non-functional.
#' @note Requires helper functions: stdz(), permutation_entropy(), ordinal_pattern_distribution()
#' @examples
#' obs=runif(100, 0.8, 1.2)
#' hat=obs * exp(rnorm(100, sd=0.15))
#' diagnostics(obs, hat)
#' @export
diagnostics<-function(obs,hat,ndemb=5){

  roc=rocFn(stdz(obs)>1,stdz(hat))
  tss=skillScore(stdz(hat),stdz(obs)-1)
  
  return(
    data.frame(
      trend     =cor(hat, obs),
      status    =mean((hat<1)== (obs<1)),
      sd.hat    =sd(hat),
      sd.obs    =sd(obs),
      variability=sd(hat)/sd(obs),
      auc       =FLCore:::auc(TPR=roc$TPR,FPR=roc$FPR),
      tss       =tss$TSS,
      fpr       =tss$FPR,
      tpr       =tss$TPR,
      entropy   =permutation_entropy(ordinal_pattern_distribution(obs, ndemb=ndemb))))
  
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
TSS<-function(TP,TN,FP,FN) TP/(FN+TP) - TN/(FP+TN)


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
#' obs=runif(100, 0.5, 1.5)
#' pred=obs * exp(rnorm(100, sd=0.2))
#' PN(obs < 1, pred < 1)
#' @export
PN<-function(x,y) {
  data.frame(TP=sum(x > 0 & y > 0),
             TN=sum(x <=0 & y <=0),
             FP=sum(x > 0 & y <=0),
             FN=sum(x <=0 & y > 0))}

#' @title Stock Assessment Skill Visualizer
#' @description Generates diagnostic plots for management procedure evaluation.
#' @param x Data.frame containing assessment results
#' @param state Character name of column with true stock status
#' @param ind Character name of column with predicted status
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
plotSkill<-function(object, state, ind, threshold=1, reference=1, xLabel="", limits=c(0,5)) {
  
    dat=transform(object,
                  state=eval(sym(state)),
                  ind  =eval(sym(ind)))
    
    dat=subset(   dat, !is.na(state)&!is.na(ind))
    dat=transform(dat, ratio=(ind-state)/state)
    smry=ddply(cbind(dat,threshold=threshold), .(Scenario), with, {
      om=state>threshold
      roc1=rocFn(om,ind)
      AUC=with(roc1,auc(TPR=TPR,FPR=FPR))
      TPR=roc1$TPR
      FPR=roc1$FPR
      ref=roc1$ind
      flag=min((ref-1)^2)==(ref-1)^2
      flg2=(TPR-FPR)==max(TPR-FPR)
      
      rtn=data.frame(AUC=AUC,
                      TSS=((TPR-FPR)[flag])[1],
                      BSS=((TPR-FPR)[flg2])[1],
                      ref=ref[flg2][1],
                      TPR=((TPR)[flag])[1],
                      FPR=((TPR)[flag])[1],
                      TPR2=TPR[flg2][1],
                      FPR2=FPR[flg2][1])
      rtn})
    
    rocDat=ddply(dat, .(Scenario), with, FLCandy:::tryIt(rocFn(state>1,ind)))
    
    # ggridges plot
    dt2=melt(dat,c("Scenario"),c("state","ind"))
    dt2$variable=factor(dt2$variable,levels=c("state","ind"),
                                     labels=c("Operating\nModel","Indicator"))
    p1=ggplot(dt2, aes(x=value, y=variable, fill=variable)) +
      geom_density_ridges(alpha=0.5, scale=1.2, rel_min_height=0.01) +
      facet_grid(Scenario~., scales="free") +
      geom_vline(xintercept=1, color="red") +
      geom_vline(aes(xintercept=ref),data=smry,col="blue") +
      scale_y_discrete(expand=c(0, 0)) +
      #scale_x_continuous(limits=limits) +
      #geom_label(aes(label=paste("TSS=",round(TSS, 2))), fill="white", #fnTss(TSS),
      #           x=Inf, y=-Inf,
      #           hjust=1, vjust=0,
      #           size=4,data=smry)+
      theme_bw()+
      theme(legend.position="none",
            plot.title=element_text(hjust=0),
            strip.text.x=element_text(angle=0))+
      labs(title = "Distribution",
           x="", 
           y="")
    
  
    # Predictions plot
    p2=ggplot(dat[sample(seq(dim(dat)[1]),pmin(dim(dat)[1],1000)),])+
      facet_grid(Scenario~.)+
      geom_point(aes(state,ind), size=1.25, alpha=0.75)+
      geom_vline(xintercept=1)+
      geom_hline(aes(yintercept=1), col="red")+
      geom_hline(aes(yintercept=ref),data=smry,col="blue")+
      #scale_x_continuous(limits=limits)+
      #scale_y_continuous(limits=limits)+
      geom_label(aes(x=Inf, y=ref,
                 label=paste("BSS=",round(BSS,2))),fill="white",#fnTss(BSS)), 
                 hjust=1, vjust=0,
                 size=4.0, col="blue",
                 data=smry)+
      geom_label(aes(x=limits[1], y=1,
                 label=paste("TSS=",round(TSS,2))),fill="white",#fnTss(TSS)), 
                 hjust=0, vjust=0, 
                 size=4.0, col="red",
                 data=smry)+
      scale_x_log10()+scale_y_log10()+
      theme_bw()+
      theme(legend.position="none",
            plot.title=element_text(hjust=0))+
      geom_label(aes(x=10^(log10(max(dat$state, na.rm = TRUE))), 
                     y=10^(log10(max(dat$ind,   na.rm = TRUE)))),label="TP",hjust=0.5,vjust= 0.5,size=4)+
      geom_label(aes(x=10^(log10(min(dat$state, na.rm = TRUE))), 
                     y=10^(log10(max(dat$ind,   na.rm = TRUE)))),label="FP",hjust=0,  vjust= 0.5,size=4)+
      geom_label(aes(x=10^(log10(max(dat$state, na.rm = TRUE))), 
                     y=10^(log10(min(dat$ind,   na.rm = TRUE)))),label="FN",hjust=0.5,vjust=-0.75,size=4)+
      geom_label(aes(x=10^(log10(min(dat$state, na.rm = TRUE))), 
                     y=10^(log10(min(dat$ind,   na.rm = TRUE)))),label="TN",hjust=0,  vjust=-0.75,size=4)+
      labs(title = "Confusion Matrix",
           x="Operating Model", 
           y="Indicator")
    
    p3=ggplot(rocDat)+
      facet_grid(Scenario~.)+
      geom_path( aes(FPR, TPR), alpha=0.5)+
      geom_point(aes(FPR, TPR), data=smry,col="red",  size=3)+
      geom_point(aes(FPR2,TPR2),data=smry,col="blue", size=3)+
      geom_abline(intercept=0, slope=1, linetype=2, linewidth=1.0, col="grey74")+
      geom_label(data=smry,
                 aes(label=paste("AUC=",round(AUC, 2))),
                 fill="white",#fnAuc(AUC),
                 x   =1.0, y   =0,
                 hjust=1,   vjust=0,
                 size=4)+
      labs(title="ROC Curve",
           x="FPR (1-Specificity)",
           y="TPR (Sensitivity)")+
      theme_bw()+
      theme(legend.position="none",
          plot.title=element_text(hjust=0))
    
    
    # Modify your plots
    p1=p1+FLCandy:::theme_no_title
    p2=p2+FLCandy:::theme_no_title
    p3=p3
    
    # Combine plots with minimal spacing
    combined=ggarrange(p1,p2,p3,
                       ncol=3, 
                       widths=c(1.25, 1, 1),
                       common.legend=TRUE,
                       legend="none",
                       align="h")
    
    #combined=annotate_figure(combined,
    #                         top=text_grob("Assessment Performance Metrics", 
    #                                       face="bold", size=14))
    
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
#' obs=cumsum(rnorm(20))
#' pred=obs + rnorm(20, sd=0.5)
#' compareTS(obs, pred)
#' @export
compareTS<-function(ts1, ts2) {

    # Ensure same length
    min_length=min(length(ts1), length(ts2))
    ts1=ts1[1:min_length]
    ts2=ts2[1:min_length]
    
    # Basic statistics
    rmse=sqrt(mean((ts1 - ts2)^2))
    correlation=cor(ts1, ts2)
    
    # Return results as list
    results=data.frame(rmse      =rmse,
                       correlation=correlation,
                       sd        =var(ts2-ts1)^0.5)
    
    return(results)}

#' @title Optimal Lag Finder
#' @description Identifies lag with maximum cross-correlation between stock assessment time series.
#' @param obs Observed time series (e.g., survey index)
#' @param hat Predicted time series (e.g., model output)
#' @param lag.max Maximum lag to consider (default=5)
#' @return Data.frame with optimal lag and corresponding ACF value
#' @examples
#' obs=sin(seq(0, 2*pi, length=50)) + rnorm(50)
#' hat=lag(obs, 2) + rnorm(50, sd=0.2)
#' ccfFn(obs, hat)
#' @export
ccfFn<-function(obs, hat, lag.max=5){
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
      scale_x_continuous(expand=c(0, 0))+
      scale_y_continuous(expand=c(0, 0))+
      labs(x="Standard Deviation", y="σ")
    
    # Add correlation contours
    angles=seq(0, ifelse(full, pi, pi/2), length.out=n_lines)
    radii=seq(min_R, max_R, length.out=contours)
    
    # Add reference circles
    for(r in radii) {
      circle_data=data.frame(
        x=r * cos(angles),
        y=r * sin(angles)
      )
      p=p + geom_path(data=circle_data, aes(x, y), 
                      linetype="dashed", color="gray70")
    }
    
    return(p)}

cmKobe<-function(stock.om,harvest.om,stock.mp,harvest.mp,what=c("red","green","yellow","orange")){
  
  dt1=prob(stock.om,harvest.om)
  dt1$yellow=dt1$yellow-dt1$orange
  dt1=suppressMessages(melt(dt1[,what]))
  dt1=suppressWarnings(cbind(dt1,row=rep(seq(length(stock.om)),length(what))))
  dt1=subset(dt1,value==1)
  dt1=dt1[order(dt1$row),c("row","variable")]
  
  dt2=prob(stock.mp,harvest.mp)
  dt2$yellow=dt2$yellow-dt2$orange
  dt2=suppressMessages(subset(melt(dt2[,what])))
  dt2=suppressWarnings(cbind(dt2,row=rep(seq(length(stock.mp)),length(what))))
  dt2=subset(dt2,value==1)
  dt2=dt2[order(dt2$row),c("row","variable")]
  
  dt=merge(dt1,dt2,by=c("row"))
  names(dt)[2:3]=c("om","mp")
  cm(dt[,"om"],dt[,"mp"])}


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
#' obs=runif(100, 0.5, 1.5) < 1
#' pred=obs + rnorm(100, sd=0.3)
#' roc_data=rocFn(obs, pred)
#' @export
rocFn<-function(labels, ind) {
  ord           =order(ind, decreasing=TRUE)  
  labels_ordered=labels[ord]
  data.frame(
    TPR   =cumsum( labels_ordered)/sum( labels_ordered),
    FPR   =cumsum(!labels_ordered)/sum(!labels_ordered),
    labels=labels_ordered,
    ind   =ind[ord])}

#' @title Calculate Prediction Skill Scores
#' @description Computes threshold-based skill scores including TSS (True Skill Statistic) and AUC (Area Under the Curve) for fishery stock assessment models.
#' @param x Numeric vector of model predictions (e.g., estimated stock biomass)
#' @param y Numeric vector of observed values (e.g., survey biomass index)
#' @param reference value for status classification (default=1, typical BMSY threshold)
#' @param threshold for classification of ind, if not provided optimised, i.e. tuned
#' @return Data.frame containing optimal threshold and associated metrics:
#' \itemize{
#'   \item ref: Optimal reference value
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
#' obsStatus=rlnorm(100, meanlog=log(1), sdlog=0.5)
#' predStatus=obsStatus * exp(rnorm(100, sd=0.3))
#' skillScore(predStatus, obsStatus > 1, reference=1)
#' @importFrom FLCore auc
#' @export
skillScore<-function(state, ind, reference = NULL, threshold = 1) {
  rocs=rocFn(state > threshold, ind)
  
  if (is.null(reference)) {
    flag=which.max(rocs$TPR - rocs$FPR)
    reference=rocs$ind[flag]
  } else {
    flag=which.min(abs(rocs$ind-reference))  # Find closest threshold
  }
  
  TP=sum(state > threshold  & ind > rocs$ind[flag])
  TN=sum(state <= threshold & ind <=rocs$ind[flag])
  FP=sum(state > threshold  & ind <=rocs$ind[flag])
  FN=sum(state <= threshold & ind > rocs$ind[flag])
  
  data.frame(
    AUC = auc(rocs$TPR, rocs$FPR),
    TSS = rocs$TPR[flag] - rocs$FPR[flag],
    ref = rocs$ind[flag],
    TPR = rocs$TPR[flag],
    FPR = rocs$FPR[flag],
    TP = TP, TN = TN, FP = FP, FN = FN)}

#' @title Stock Assessment Model Summary
#' @description Computes comprehensive performance metrics for fishery management procedures.
#' @param state Numeric vector of operating model outputs (true values)
#' @param ind Numeric vector of management procedure predictions
#' @return Data.frame containing:
#' \itemize{
#'   \item AUC: Area Under ROC Curve
#'   \item TSS: True Skill Statistic at reference threshold
#'   \item BSS: Best achievable skill score
#'   \item best: optimal reference level to get BSS
#'   \item TPR/FPR: Rates at reference threshold
#'   \item TPR2/FPR2: Rates at optimal threshold
#' }
#' @examples
#' state=runif(100, 0.5, 1.5)
#' ind=state * exp(rnorm(100, sd=0.2))
#' skillSummary(state>1, ind)
#' @export
skillSummary<-function(label, ind, reference = NULL) {
  rocs=rocFn(label, ind)
  
  if (is.null(reference)) {
    flag=which.max(rocs$TPR - rocs$FPR)
  } else {
    flag=which.min(abs(rocs$ind-reference))
  }
  
  data.frame(
    AUC = auc(rocs$TPR, rocs$FPR),
    TSS = rocs$TPR[flag] - rocs$FPR[flag],
    ref = rocs$ind[flag],
    TPR = rocs$TPR[flag],
    FPR = rocs$FPR[flag]
  )}


skillSummaryOld<-function(om,mp) {
  roc1 = rocFn(om, mp)
  AUC = with(roc1, FLCore:::auc(TPR = TPR, FPR = FPR))
  TPR = roc1$TPR
  FPR = roc1$FPR
  ref = roc1$reference
  flag = min((ref - 1)^2) == (ref - 1)^2
  flg2 = (TPR - FPR) == max(TPR - FPR)
  rtn = data.frame(AUC = AUC, TSS = ((TPR - FPR)[flag])[1], 
                   BSS = ((TPR - FPR)[flg2])[1], ref = ref[flg2][1], TPR = TPR[flag][1], 
                   FPR = FPR[flag][1], TPR2 = TPR[flg2][1], FPR2 = FPR[flg2][1])
  rtn}


cm<-function(hat,obs){
  if (is.logical(hat)) hat=factor(hat,levels=c("FALSE","TRUE"))
  if (is.logical(obs)) obs=factor(obs,levels=c("FALSE","TRUE"))
  
  # ------------------------------------------------------------------------------
  # Step 2: Compute the confusion matrix with evaluation metrics
  # ------------------------------------------------------------------------------
  CM=confusionMatrix(hat, obs, mode="everything")
  
  # Overall accuracy and Cohen's kappa
  accuracy=CM$overall["Accuracy"]
  kappa   =CM$overall["Kappa"]
  #return(CM$overall)
  # ------------------------------------------------------------------------------
  # Step 3: Extract per-class metrics (precision, recall, F1-score)
  # ------------------------------------------------------------------------------
  if (is.null(dimnames(CM$byClass)[[1]])){
    TSS=CM$byClass["Sensitivity"]+CM$byClass["Specificity"]-1
    names(TSS)="TSS"
    return(c(CM$byClass,TSS))} 
  
  class=gsub("Class: ","",dimnames(CM$byClass)[[1]])
  
  transform(data.frame("class"=class,CM$byClass),TSS=unlist(Sensitivity+Specificity-1))}

if (FALSE){
  library(MASS)
  library(caret)
  
  om=as.data.frame(mvrnorm(100,c(1,1),matrix(c(0.6,0.3,0.3,0.6),2,2)))
  names(om)=c("stock.om","harvest.om")
  ommp=transform(om,stock.mp=stock.om*rlnorm(100),harvest.mp=harvest.om*rlnorm(100))
  
  with(ommp,skillScore(  stock.om,  stock.mp, reference=1))
  with(ommp,skillSummary(stock.om>1,stock.mp, reference=1))
  with(ommp,cm(          stock.om>1,stock.mp>1))

  with(ommp,rocFn(stock.om>1,stock.mp))[47:48,]

  with(ommp,table(stock.om>1,stock.mp>1))
  with(ommp,confusionMatrix(factor(stock.om>1),factor(stock.mp>1), mode="everything"))[[2]]
  
  with(ommp,skillScore(  harvest.om,  harvest.mp, reference=1))
  with(ommp,skillSummary(harvest.om>1,harvest.mp))
  with(ommp,cm(          harvest.om>1,harvest.mp>1))
  }