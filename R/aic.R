# Function to calculate AIC weights
aicWts<-function(logLiks) {
  # Extract log-likelihoods and degrees of freedom
  values=sapply(logLiks,function(x) x[1])
  df    =sapply(logLiks, function(x) x[[2]])
  aics  =-2*values+2*df
  
  
  # Calculate delta AIC
  delta=aics - min(aics)
  
  # Calculate AIC weights
  wts =exp(-0.5*delta)/sum(exp(-0.5*delta))
  
  # Return results as a data frame
  return(data.frame(model=names(logLiks), AIC=aics, deltaAIC=delta, weight=wts))}
