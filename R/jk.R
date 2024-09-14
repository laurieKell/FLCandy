# Load necessary libraries
require(dplyr)
require(bootstrap)

# Function to apply jackknife  vector of data
jkLmean<-function(dat) {
  
  # Function to calculate lmean
  Fn<-function(dat, indices) {
    
    len=dat[indices]
    
    lc=lmodeFn(len,1)*0.5
    
    lmean=weighted.mean(len,1*(len>=lc))
    
    return(lmean)}
  
  # Apply the jackknife method
  res=jackknife(dat, Fn)
  
  return(res)}

jk  =dlply(len, .(sp, gender), with, jkLmean(len))
```