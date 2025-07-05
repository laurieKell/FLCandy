#' @title ICES RFB Rule Control Parameters
#' 
#' @description Sets up control parameters for ICES RFB (r × f × b × m) harvest control rule according to ICES Technical Guidelines for Category 3 stocks.
#'
#' @param linf Numeric. Von Bertalanffy asymptotic length (cm)
#' @param lc Numeric. Length at first capture (cm)
#' @param k Numeric. Von Bertalanffy growth coefficient (year⁻¹)
#' @param index FLQuant. Historical biomass index values
#' 
#' @return FLPar containing three parameters:
#' \itemize{
#'   \item `L[F]=M` - Length at F=M reference (0.75*lc + 0.25*linf)
#'   \item `Itrig` - Biomass trigger (1.4 × minimum historical index)
#'   \item `multiplier` - Precautionary multiplier (0.95 if k < 0.2, 0.90 otherwise)
#' }
#' 
#' @details
#' Implements Equations 6.11 (L[F]=M) and 6.12 (Itrig) from ICES Technical Guidelines. The multiplier follows Table 6.3 based on von Bertalanffy k values.
#'
#' @seealso \code{\link{rfbRule}}
#' 
#' @examples
#' \dontrun{
#' # For a stock with L∞=35cm, Lc=25cm, k=0.25
#' cntrl <- rfbParams(linf=35, lc=25, k=0.25, index=historical_index)
#' }
#' 
#' @export
rfbParams<-function(linf,lc,k,index) {
  
  # Calculate L[F]=M (0.75*lc+0.25*linf)
  lfm=0.75*lc+0.25*linf
  
  # Calculate Itrig (1.4*minimum observed index)
  Itrig=1.4*min(index,na.rm = TRUE)
  
  # Determine multiplier based on von Bertalanffy k
  multiplier=ifelse(k<0.2,0.95,0.90)
  
  return(FLPar(c(
    "L[F]=M"    =lfm,
    "Itrig"     =Itrig,
    "multiplier"=multiplier)))}


#' @title ICES RFB Harvest Control Rule
#' 
#' @description Applies the ICES RFB (r × f × b × m) harvest control rule to calculate catch advice for data-limited stocks.
#'
#' @param iYr Numeric. Current management year
#' @param indx FLQuant. Biomass index time series
#' @param FIndx FLQuant. Length-based fishing pressure index (mean length in catch)
#' @param tac FLQuant. Previous catch advice time series
#' @param cntrl FLPar. Control parameters from \code{rfbParams}
#' @param bnd Numeric vector (length 2). Stability clause bounds [lower, upper]
#' @param ncur Numeric. Number of current years for r calculation (default=2)
#' @param npast Numeric. Number of historical years for r calculation (default=3)
#' 
#' @return FLQuant containing new catch advice for year \code{iYr}
#' 
#' @details
#' Implements the full RFB rule as:
#' \deqn{A_{y+1} = A_y \times r \times f \times b \times m}
#' Where:
#' \itemize{
#'   \item \code{r}: 2-over-3 biomass trend ratio
#'   \item \code{f}: Length-based fishing pressure proxy (Lmean/L[F]=M)
#'   \item \code{b}: Biomass safeguard (min(1, I_{y-1}/I_trigger))
#'   \item \code{m}: Precautionary multiplier
#' }
#' Applies stability clause (bnd) only when biomass is above trigger (b=1).
#' 
#' @section Validation:
#' Checks for:
#' \itemize{
#'   \item Consistent iteration dimensions
#'   \item Valid year alignment in input FLQuants
#'   \item Presence of required control parameters
#' }
#' 
#' @references ICES (2025). Technical Guidelines for Data-limited Stock Assessments. ICES CM 2025/ACOM:68
#' 
#' @seealso \code{\link{rfbParams}}
#' 
#' @examples
#' \dontrun{
#' # Using FLR objects from ICES WKMSY:
#' data(icesdata)
#' stk <- icesdata[[1]]
#' cntrl <- rfbParams(linf=35, lc=25, k=0.25, index=ebiomass(stk))
#' 
#' # Project forward
#' for(iYr in 2020:2030) {
#'   tac[,ac(iYr)] <- rfbRule(iYr, indx=ebiomass(stk), 
#'                           FIndx=1/fbar(stk), tac=tac, cntrl=cntrl)
#' }
#' }
#' 
#' @export

# RFB rule
rfbRule<-function(iYr,indx,FIndx,tac,cntrl,bnd=c(0.7,1.2),ncur=2,npast=3) {
  # Validate inputs
  if (missing(iYr) || !is.numeric(iYr))
    stop("iYr must be numeric")
  
  ### Iters
  nits=unique(c(dim(indx)[6],dim(FIndx)[6],dim(tac)[6]))
  if (length(nits)>2)  stop("iter must be 1 or n")
  if (length(nits)!=1 & min(nits)>1) stop("iter must be 1 or n")
  
  ### FLQuants
  if (unique(dim(indx)[c(1,3,4,5)])!=1)
    stop("Cannot have ages, units or seasons indx")
  if (unique(dim(FIndx)[c(1,3,4,5)])!=1)
    stop("Cannot have ages, units or seasons FIndx")
  if (unique(dim(tac)[c(1,3,4,5)])!=1)
    stop("Cannot have ages, units or seasons in tac")
  
  ### Years  
  if (!(iYr%in%dimnames(tac)$year))
    stop(paste(iYr,"not in in tac"))
  if (!(iYr%in%dimnames(FIndx)$year))
    stop(paste(iYr,"not in in indx"))
  if (!all((iYr-(1:(ncur+npast)))%in%dimnames(indx)$year))
    stop(paste(iYr,"-", ncur+npast, "not in in index"))
  
  # Biomass Trend Component (r) 
  # F Proxy (f)
  yrKey=as.character(iYr-1)
  if (!yrKey %in% dimnames(FIndx)$year)
    stop("Length indicator missing for year: ",yrKey)
  Lmean=apply(FIndx[,yrKey],6,mean,na.rm=TRUE)
  
  # Validate control parameter
  if (!"L[F]=M" %in% dimnames(cntrl)$params)
    stop("Parameter 'L[F]=M' missing in cntrl.")
  
  f=Lmean%/%cntrl["L[F]=M"]
  
  # Biomass Safeguard (b)
  # Extract index for previous year
  curIndex=indx[,yrKey]
  
  if (!"Itrig" %in% dimnames(cntrl)$params)
    stop("Parameter 'Itrig' missing in cntrl.")
  Itrig=cntrl["Itrig"]
  b=qmin(curIndex%/%Itrig,1)
  
  #  Calculate Advice
  advice=tac[,yrKey]%*%r%*%f%*%b%*%cntrl["multiplier"]
  
  # Apply Stability Clause (when b=1)
  flag=b==1
  if (any(flag)) {
    minAdv=advice[,,,,,flag]*bnd[1]
    maxAdv=advice[,,,,,flag]*bnd[2]
    advice[,,,,,flag]=qmax(qmin(advice[,,,,,flag],maxAdv),minAdv)}
  
  return(advice)}

#' @title ICES RFB Rule Simulation Example
#' 
#' @description Demonstrates full RFB rule implementation using FLR framework objects.
#' Requires ICES WKMSY data package for stock and SRR objects.
#' 
#' @param id Numeric. Stock ID from icesdata list
#' @param endYear Numeric. Final projection year
#' @param nits Numeric. Number of iterations
#' 
#' @return Invisible. Generates two diagnostic plots:
#' \itemize{
#'   \item Stock trajectory plot
#'   \item Indicator time series plot
#' }
#' 
#' @details
#' Shows complete workflow for:
#' \itemize{
#'   \item Parameter setup with \code{rfbParams}
#'   \item Annual advice calculation with \code{rfbRule}
#'   \item Forward projection with \code{fwd}
#'   \index{Diagnostic plotting}
#' }
#' 
#' @examples
#' \dontrun{
#' # Run simulation for North Sea cod (id=1)
#' simulateRFB(id=1, endYear=2050, nits=100)
#' }
#' 
#' @importFrom FLCore propagate fwdWindow fwd
#' @import ggplot2
simulateRFB <- function(id=1, endYear=2050, nits=100) {

  # Life-history parameters
  linf=35   # asymptotic length
  lc  =25   # length at first capture
  k   =0.25 # growth rate
  
  id=1
  stk      =icesdata[[id]]
  eql      =bhs[[id]]
  startYear=dims(stk)$maxyear
  endYear  =2050
  nits     =100
  set.seed(23)
  
  stk=fwdWindow(stk,eql,end=endYear+1)
  stk=propagate(stk,nits)
  
  index    =ebiomass(stk)
  indexDevs=rlnorm(nits,index%=%0,0.2)
  lbi      =1/fbar(stk)
  lbiDevs  =rlnorm(nits,lbi%=%0,0.2)
  
  recDevs=attributes(eql)$rec.residuals
  recDevs=recDevs[,rev(dim(recDevs)[2]-0:min(dim(recDevs)[2]-1,20))]
  recDevs=recDevs-median(recDevs)
  recDevs=FLQuant(sample(c(recDevs),dim(recDevs)[2]*nits,TRUE),
                  dimnames=list(year=dims(icesdata[[id]])$maxyear:endYear,iter=seq(nits)))
  recDevs=exp(recDevs)
  
  tac=catch(stk)
  
  # Control parameters
  cntrl=rfbParams(linf,lc,k,index)
  cntrl[1]=1/refpts(eql)["msy","harvest"]
  
  for (iYr in (2020):(endYear)){
    tac[,ac(iYr)]=rfbRule(iYr,indx =index,
                          FIndx=lbi,
                          tac  =tac,
                          cntrl=cntrl)
    
    stk=fwd(stk,catch=tac[,ac(iYr)],sr=eql,residuals=recDevs)
    
    index[,ac(iYr)]=ebiomass(stk)[,ac(iYr)]%*%indexDevs[,ac(iYr)]
    lbi[  ,ac(iYr)]=1/fbar(    stk)[,ac(iYr)]%*%lbiDevs[,ac(iYr)]}
  
  plot(stk,iter=1)+
    scale_x_continuous(limits=c(2010,2050))
  
  plot(FLQuants(index=index,lbi=lbi,tac=catch(stk),fbar=fbar(stk)))+
    scale_x_continuous(limits=c(2010,2050))
  }