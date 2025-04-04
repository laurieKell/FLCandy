library(rlang)      # for dynamic evaluation (i.e., sym)
library(plyr)       # for ddply and dlply/ldply
library(ggplot2)    # for plotting
library(ggpubr)     # for ggarrange and annotate_figure

skillScore<-function(x, y, threshold=NULL, reference=1) {

    # If no thresholds provided, use the unique sorted values of y
    if (is.null(threshold))
       threshold=sort(unique(y))
    
    # Internal function to compute AUC using a trapezoidal rule
    computeAUC<-function(TPR, FPR) {
          ord=order(FPR)
          TPR=TPR[ord]
          FPR=FPR[ord]
          # Standard trapezoidal integration
          sum(diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2)}
          
    # Compute performance metrics over all candidate thresholds
    results=sapply(threshold, function(thresh) {
        TP=sum(x >= reference & y >= thresh)
        TN=sum(x <  reference & y <  thresh)
        FP=sum(x <  reference & y >= thresh)
        FN=sum(x >= reference & y <  thresh)
        
        TPR=if ((TP + FN) > 0) TP/(TP + FN) else 0    # Sensitivity
        FPR=if ((FP + TN) > 0) FP/(FP + TN) else 0    # 1 - Specificity
        TSS=TPR - FPR                                 # True Skill Statistic
        c(TP=TP, TN=TN, FP=FP, FN=FN, TPR=TPR, FPR=FPR, TSS=TSS)})
    
    # Create a data frame that contains the ROC curve for all thresholds
    roc_df=data.frame(threshold= threshold,
                      TPR     =as.numeric(results["TPR", ]),
                      FPR     =as.numeric(results["FPR", ]))
    
    # Find the threshold that gives the maximum TSS
    maxIdx=which.max(results["TSS", ])
    
    # Compute AUC over the full ROC curve
    auc_val=computeAUC(roc_df$TPR, roc_df$FPR)
    
    # Create a summary data frame of optimal metrics using the best threshold
    optimal=data.frame(ref=threshold[maxIdx],
                      TP =results["TP", maxIdx],
                      TN =results["TN", maxIdx],
                      FP =results["FP", maxIdx],
                      FN =results["FN", maxIdx],
                      TSS=results["TSS", maxIdx],
                      TPR=results["TPR", maxIdx],
                      FPR=results["FPR", maxIdx],
                      AUC=auc_val)
    
    # Return a list containing both the optimal summary and the full ROC data
    return(list(optimal=optimal, roc=roc_df))}

createPlots=function(x,
                     state,
                     indicator,
                     xLabel  ="",
                     limits  =c(0, 5),
                     threshold=NULL,
                     reference=1) { 

      # Dynamically evaluate the columns for state and indicator
      dat=transform(x,
      state    =eval(sym(state)),
      indicator=eval(sym(indicator)))
      
      # Remove rows with missing values and add a bias ratio column
      dat=subset(dat, !is.na(state) & !is.na(indicator))
      dat=transform(dat, ratio=(indicator - state) / state)
      
      # Calculate performance metrics for each Scenario group.
      
      # The returned list per group has two components: optimal (summary) and roc (curve)
      
      scores_list=dlply(dat, .(Scenario), function(df) {
      skillScore(x=df$state, y=df$indicator, threshold=threshold, reference=reference)
      })
      
      # Extract the optimal metrics into one summary data frame
      
      smry=ldply(scores_list, function(res) res$optimal)
      
      # Also combine all ROC curve data with Scenario information
      
      rocDat=ldply(scores_list, function(res) res$roc)
      
      # --------------------------- PLOT 1 --------------------------------
      
      # Density Plot: state vs. indicator distributions
      
      p1=ggplot(dat) +
      geom_density(aes(x=state), fill="green", alpha=0.5) +
      geom_density(aes(x=indicator), fill="#E69F00", alpha=0.5) +
      facet_grid(Scenario ~ ., scales="free") +
      geom_vline(xintercept=reference, linetype="dashed", color="black") +
      geom_label(data=smry,
      aes(x=Inf, y=-Inf, label=paste("TSS=", round(TSS, 2))),
      hjust=1, vjust=-0.5,
      size=4, alpha=0.6) +
      scale_y_continuous(expand=c(0, 0)) +
      scale_x_continuous(limits=limits) +
      labs(title="Density Plot", x=xLabel) +
      theme_minimal()
      
      # --------------------------- PLOT 2 --------------------------------
      
      # Bias Plot: Boxplot of the ratio (bias) between indicator and state
      
      p2=ggplot(dat, aes(x="", y=ratio)) +
      geom_boxplot(fill="#E69F00", alpha=0.5) +
      coord_flip() +
      geom_vline(xintercept=0, color="red") +
      facet_grid(Scenario ~ .) +
      coord_cartesian(xlim=c(-1, 5)) +
      labs(title="Bias Plot",
      x=xLabel,
      y="Bias (Indicator - State)/State") +
      theme_minimal()
      
      # --------------------------- PLOT 3 --------------------------------
      
      # Predictions Plot: Scatter plot with optimal indicator threshold annotated.
      
      # For performance, a random sample of up to 1000 points is used.
      
      set.seed(123)  # For reproducibility of the sample
      sample_idx=sample(seq_len(nrow(dat)), size=min(nrow(dat), 1000))
      p3=ggplot(dat[sample_idx, ], aes(x=state, y=indicator)) +
      geom_point(size=2.5, alpha=0.6) +
      facet_grid(Scenario ~ .) +
      geom_vline(xintercept=reference, linetype="dashed", color="black") +
      geom_hline(yintercept=1, linetype="dashed", color="blue") +
      # Horizontal line at the optimal indicator threshold from skillScore
      geom_hline(data=smry, aes(yintercept=ref),
      color="red", linetype="dotted") +
      scale_x_continuous(limits=limits) +
      scale_y_continuous(limits=limits) +
      labs(title="Predictions Plot",
      x="State",
      y="Indicator") +
      geom_label(data=smry,
      aes(x=limits, y=ref,
      label=paste("TSS=", round(TSS, 2))),
      hjust=0, vjust=0, size=4, color="red") +
      theme_minimal() +
      scale_x_log10() + scale_y_log10()
      
      # --------------------------- PLOT 4 --------------------------------
      
      # ROC Curve Plot: Plot the full ROC curve per Scenario and mark the optimal point.
      
      p4=ggplot(rocDat, aes(x=FPR, y=TPR)) +
      geom_path(alpha=0.5) +
      facet_grid(Scenario ~ .) +
      geom_abline(intercept=0, slope=1,
      linetype="dashed", size=1.0, color="grey50") +
      geom_point(data=smry, aes(x=FPR, y=TPR),
      color="blue", size=2) +
      geom_label(data=smry,
      aes(x=1, y=0.2,
      label=paste("AUC=", round(AUC, 2))),
      hjust=1, vjust=1, size=4) +
      labs(title="ROC Curve",
      x="False Positive Rate (1 - Specificity)",
      y="True Positive Rate (Sensitivity)") +
      theme_minimal()
      
      # --------------------------- COMBINE PLOTS ----------------------------
      
      combined=ggarrange(p1, p3, p4,
      ncol=3,
      widths=c(1.25, 1, 1),
      common.legend=TRUE)
      
      combined=annotate_figure(combined,
      top=text_grob("Assessment Performance Metrics",
      face="bold", size=14))
      
      return(combined)
      }
# 
# final_plot=createPlots(myData,
# state="StateColumn",
# indicator="IndicatorColumn",
# xLabel="Your X-axis Label",
# limits=c(0.1, 10),
# threshold=NULL,  # or provide a numeric vector of thresholds
# reference=1)
# print(final_plot)

