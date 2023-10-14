## par on normal scale,
## cor is the correlation matrix on natural scale
## sigma is the CV on natural scale
rlmvn<-function(n,par,sigma,cor){
  flag=par<0
  par[flag]=-par[flag]
  cvr=cor2cov(cor[drop=T],sigma[drop=T]^2)
  mvlmu=log(par[drop=T]) 
  rtn=exp(mvtnorm::rmvnorm(n, mean=mvlmu[drop=T], sigma=cvr[drop=T], method = c("svd")))
  rtn=FLPar(t(array(rtn,c(n,dim(par)[1]),dimnames=list(iter=seq(n),params=dimnames(par)[[1]]))))
  rtn[flag]=-rtn[flag]
  rtn}

#' \dontrun{
#'   par    =FLPar(c(linf=8,k=.7))
#'   cor    =FLPar(array(c(1,-0.115,-0.115,1),c(2,2),dimnames=list(params=c("linf","k"),params=c("linf","k"))))
#'   sigma  =par*0+0.1
#'   pr2=rlmvn(100000,par,sigma,cor)
#'   
#'   plot(pr2)
#'   
#'   ggpairs(model.frame(pr2)[,-3])
#' }