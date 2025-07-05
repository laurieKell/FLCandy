
covar<-function(path,file="covar.sso"){

  covarFile=file.path(path,file)
  
  # Read enough lines to find where the matrix starts
  covarLines=readLines(covarFile, n=200)
  startLine =grep("active-i", covarLines)-1
  if(length(startLine)==0) 
    stop("Could not find 'active-i' in covar.sso")
  
  # Read the covariance matrix
  covar=read.table(covarFile,header=TRUE,skip=startLine)
  
  # Filter for parameter-parameter covariances
  covarPar=subset(covar, Par..i=="Par" & Par..j=="Par")
  diag    =subset(covar, Par..i=="Par" & Par..j=="Std")
  
  # Get unique parameter names and matrix size
  parNames_i=unique(covarPar$label.i)
  parNames_j=unique(covarPar$label.j)
  parNames  =c(unique(parNames_i,parNames_j))
  npar      =length(diag[,"label.i"])
  
  # Build the covariance matrix
  covmat=matrix(NA, npar, npar, dimnames=list(diag[,"label.i"], diag[,"label.i"]))
  for(i in 1:nrow(covarPar)) {
    # Adjust these column names to match your actual data
    rowname=covarPar$label.i[i]
    colname=covarPar$label.j[i]  # This name may differ in your data
    value  =covarPar$corr[i]  # Use your actual value column
    
    # Only add if names are valid
    if(!is.na(rowname)&rowname%in%parNames_i && !is.na(colname)&colname%in%parNames_j) 
      covmat[rowname, colname]=value}
  
  # Symmetrize (in case of rounding asymmetry)
  covmat[upper.tri(covmat)]=t(covmat)[upper.tri(covmat)]
  diag(covmat)=diag[,"corr"]
  
  # Remove NA rows and columns before inverting
  na_rows=apply(covmat, 1, function(x) all(is.na(x)))
  covmat=covmat[!na_rows, !na_rows]
  
  return(covmat)}

hessian<-function(covar){
  
  rtn=NULL
  
  # Check if the matrix is valid before proceeding
  if(nrow(covmat)>0 && ncol(covmat)>0) {
    # Invert to get Hessian and calculate eigenvalues
    rtn=try(solve(covmat), silent=TRUE)
    
    if(!inherits(rtn, "try-error")) {
      eigenvals=eigen(rtn, symmetric=TRUE)$values
      # print(eigenvals)
    } else {
      print("Matrix inversion failed - check for singularity")
    }
  } else {
    print("No valid data found in covariance matrix")
  }
  
  return(rtn)}
  
