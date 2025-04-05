
rocFn<-function(labels, scores){
  labels=labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels),
             FPR=cumsum(!labels)/sum(!labels),
             labels,
             reference=sort(scores))}

TSS<-function(TP,TN,FP,FN)  TP/(FN+TP) - TN/(FP+TN) 

skillScore<-function(x, y, threshold=NULL, reference=1) {
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

trend<-function(obs,hat){ 
  data.frame(
    pearson  =cor(hat, obs, method = "pearson"),
    spearman =cor(hat, obs, method = "spearman"),
    direction=mean(sign(diff(hat)) == sign(diff(obs)), na.rm = TRUE))}

state<-function(obs,hat){
  
  obs =ifelse(obs < 1, "Overfished", "Healthy")
  pred=ifelse(hat < 1, "Overfished", "Healthy")
  
  data.frame(
    accuracy  =mean(obs_status==pred_status),
    precision =sum( obs =="Overfished" & pred=="Overfished")/sum(pred=="Overfished"),
    recall    =sum( obs =="Overfished" & pred=="Overfished")/sum(obs =="Overfished"))}

variability<-function(obs,hat){
  data.frame(sd =sd(hat)/sd(obs),
             iqr=IQR(hat)/IQR(obs),
             cv =(sd(hat)/mean(hat))/(sd(obs)/mean(obs)))}

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

ccfFn<-function(obs,hat,lag.max=5){
  rtn=ccf(obs,hat,plot=FALSE,lag.max=lag.max)
  subset(data.frame(lag=rtn$lag,
                    acf=rtn$acf),acf==max(acf))}


# Function to create Taylor diagram using ggplot2
taylor_diagram_gg=function(min_R = 0.25, max_R = 1.75, contours = 7, 
                           n_lines = 10, x_0 = 1, ref_r_min = 0.25, 
                           ref_r_max = 2, ref_contours = 8, full = FALSE) {
  
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

PN<-function(x,y) {
  data.frame(TP=sum(x>=0&y>=0),
             TN=sum(x< 0&y< 0),
             FP=sum(x>=0&y< 0),
             FN=sum(x< 0&y>=0))}

rocFn<-function(labels, scores){
  labels=labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels),
             FPR=cumsum(!labels)/sum(!labels),
             labels,
             reference=sort(scores))}


# Generic function to create standardised plots
createPlots<-function(x,
                      state,
                      indicator,
                      xLabel="",
                      limits=c(0,5)) {
  
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


# Function to compare two time series
compareTS<-function(ts1, ts2) {
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
