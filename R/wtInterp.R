#' @title 
#' 
#' @description 
#'
#' @param object an \code{FLStock} object 
#' @param seasons a numeric with seasons
#' 
#' @aliases
#' 
#' @return \code{FLStock} object
#'
#' @seealso \code{\link{expand}}
#'
#' @export seasonalise
#' @docType methods
#' @rdname seasonalise
#'
#' 
#' @examples
#' \dontrun{
#' }

# wtInterp<-function(wt){
#   incmt=(-wt[-dim(wt)[1]]+wt[-1])/dim(wt)[4]
#   incmt[,,,1]=0
#   incmt=aaply(incmt,c(1:3,5:6), cumsum)
#   names(dimnames(incmt))[3]="season"
#   incmt=as.FLQuant(transmute(melt(incmt),year=year,data=value,age=age,season=season))
#   
#   wt[dimnames(incmt)$age]=wt[dimnames(incmt)$age]%+%incmt
#   wt}

if(FALSE){
tst=FLQuant(1,dimnames=list(age=1:5,season=1:4,year=2001:2010))
tst
apply(tst,c(1,2), cumsum)
}

wtInterp<-function(wt){
  d4=dim(wt)[4]
  incmt=(-wt[-dim(wt)[1]]+wt[-1])%*%FLQuant(seq(0,1,length.out=d4+1)[-(d4+1)],dimnames=list(season=seq(d4)))

  wt[dimnames(incmt)$age]=wt[dimnames(incmt)$age]%+%incmt
  wt}

wtInterpOld<-function(wt){
  
  mlt=wt[,-dim(wt)[2]]
  mlt=FLQuant(rep(seq(0,(dim(mlt)[4]-1)/dim(mlt)[4],1/dim(mlt)[4]),each=max(cumprod(dim(mlt)[1:3]))),
              dimnames=dimnames(mlt))[-dim(mlt)[1]]
  
  incmt=wt[,,,1]
  incmt=-incmt[-dim(incmt)[1],-dim(incmt)[2]]+incmt[-1,-1]
  
  wt[dimnames(incmt)$age,dimnames(incmt)$year]=
    wt[dimnames(incmt)$age,dimnames(incmt)$year]+
    incmt%+%(mlt%*%incmt)
  
  wt[,-dim(wt)[2]]}

qp<-function(stk,eql){
  
  dat=rbind.fill(
  cbind(What="Stock.wt", merge( model.frame(FLQuants("FLStock"=stock.wt(stk)),drop=T),
                      transform(model.frame(FLQuants("FLBRP"  =stock.wt(eql)),drop=T),age=age%/%4,season=age-4*(age%/%4)+1))),
  cbind(What="Catch.wt", merge( model.frame(FLQuants("FLStock"=catch.wt(stk)),drop=T),
                      transform(model.frame(FLQuants("FLBRP"  =catch.wt(eql)),drop=T),age=age%/%4,season=age-4*(age%/%4)+1))),
  cbind(What="Stock.n", merge( model.frame(FLQuants("FLStock"=stock.n(stk)),drop=T),
                      transform(model.frame(FLQuants("FLBRP"  =stock.n(eql)),drop=T),age=age%/%4,season=age-4*(age%/%4)+1))),
  cbind(What="Catch.n", merge( model.frame(FLQuants("FLStock"=catch.n(stk)),drop=T),
                               transform(model.frame(FLQuants("FLBRP"  =catch.n(eql)),drop=T),age=age%/%4,season=age-4*(age%/%4)+1))),
  cbind(What="M",       merge( model.frame(FLQuants("FLStock"=m(stk)),drop=T),
                      transform(model.frame(FLQuants("FLBRP"  =m(eql)),drop=T),age=age%/%4,season=age-4*(age%/%4)+1))),
  cbind(What="Mat",     merge( model.frame(FLQuants("FLStock"=mat(stk)),drop=T),
                      transform(model.frame(FLQuants("FLBRP"  =mat(eql)),drop=T),age=age%/%4,season=age-4*(age%/%4)+1))),
  cbind(What="Harvest", merge( model.frame(FLQuants("FLStock"=harvest(stk)),drop=T),
                      transform(model.frame(FLQuants("FLBRP"  =harvest(eql)),drop=T),age=age%/%4,season=age-4*(age%/%4)+1))))

  }

