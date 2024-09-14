load("C:/flrpapers/jabba/data/eqs.RData")  
load("C:/flrpapers/roc_ices_stocks/data/inputs/Updated_stks_n82_R0_updated202408.RData")

library(FLCore)
library(FLBRP)  
library(FLasher)
library(ggplotFL)  
library(plyr)
library(ggpubr)

pidAdjust<-function(current, ref, pid=FLPar(Kp=1,Ki=0.01,Kd=0.01,Kp2=1)) {
  
  # Calculate error terms
  error     =  current  %-% ref
  integral  <<-integral %+% error
  derivative=  error    %-% prevErr
  prevErr   <<- error
  
  kp=pid["Kp"]
  if (any(error<0)) 
     kp[error<0]=pid["Kp2"]
  
  # Calculate PID adjustment
  adjustment=(kp%*%error)%+%
             (pid["Ki" ]%*%integral)%+%
             (pid["Kd" ]%*%derivative)
  
  return(adjustment)}

stks=ldply(ICESStocks, function(x) !(any(is.na(catch.n(x)))|
                                     any(is.na(landings.n(x)))))

pid=as(expand.grid(Kp =seq(0.00,0.20, length.out=5),
                   Kp2=seq(1.00,2.00, length.out=2),
                   Ki =seq(0.00,0.00, length.out=1),
                   Kd =seq(0.00,0.00, length.out=1)),"FLPar")
pid["Kp2"]=pid["Kp"]*pid["Kp2"]


for (.id in stks$.id){

x   =ICESStocks[[.id]]
y   =eqs[[.id]]
yrs =dims(x)$maxyear+(-10:20)

fmsy=unlist(attributes(x)$benchmark["Fmsy"])
btar=unlist(attributes(x)$eqsim["BMSY"])
maxF=mean(fbar(x))*2.5
r0  =unlist(attributes(x)$eqsim["R0"])
cEq =unlist(attributes(x)$eqsim["Catchequi"])

if (any(is.na(btar))) btar = mean(ssb(x),na.rm=TRUE)

#x2=window(   x, end=dimnames(x)$year[dim(x)[2]-10])
#x2=fwdWindow(x2,end=dimnames(x)$year[dim(x)[2]],y)
#x2=fwd(      x2,catch=catch(x)[,dimnames(x)$year[dim(x)[2]-(10:0)]],sr=y)
#x3=try(fwd(  x, catch=catch(x)[,dimnames(x)$year[dim(x)[2]-(10:0)]],sr=rec(x)))

if (!"try-error"%in%is(x3)){
  
  x4   =fwdWindow(x2,end=ac(an(dimnames(x)$year[dim(x)[2]])+20),y)
  x4   =propagate(x4,dim(pid)[2])
  btar=FLQuant(btar,dimnames=dimnames(ssb(x4)[,1]))
  adj  =ssb(x4)[,ac(yrs[-1])]%=%0
  
  # Initialize static variables for integral and previous error
  integral=btar%=%0
  prevErr =btar%=%0
  
  for (iYr in an(dimnames(x4)$year[dim(x4)[2]-(29:21)])){
    adj[,ac(iYr)]=1+pidAdjust(ssb(x4)[,ac(iYr-1)]%/%btar,btar%=%1,pid=pid)
    #adj[,ac(iYr)]=qmax(qmin(adj[,ac(iYr)],1.2),0.5)
    x4=fwd(x4, catch=adj[,ac(iYr)]%*%catch(x4)[,ac(iYr-1)],sr=rec(x),maxF=maxF)
  }
  
  recDevs=FLQuant(mean(rec(x)[,ac(an(dims(x)[7])-10:0)])/mean(rec(x)), 
                  dimnames=list(year=ac(an(dims(x)[7])+0:20)))
  
  for (iYr in an(dimnames(x4)$year[dim(x4)[2]-(20:0)])){
    adj[,ac(iYr)]=1+pidAdjust(ssb(x4)[,ac(iYr-1)]%/%btar,btar%=%1,pid=pid)
    adj[,ac(iYr)]=qmax(qmin(adj[,ac(iYr)],1.2),0.5)
    x4=fwd(x4, catch=adj[,ac(iYr)]%*%catch(x4)[,ac(iYr-1)],sr=y,deviances=recDevs,maxF=maxF)
  }
  
  p1=
  plot(FLStocks("ICES"=x, "PID"=x4)) +
    geom_line( aes(year,data,group=paste(stock,iter))) +
    geom_hline(aes(yintercept = dat), col = "blue", data = data.frame(qname = "SB", dat = c(btar)[1])) +
    geom_vline(aes(xintercept = dims(x)$maxyear-9), col = "red", linetype = "dashed") +
    geom_vline(aes(xintercept = dims(x)$maxyear-1), col = "red", linetype = "dashed") +
    labs(title=.id,
         x = "",
         y = "") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),  
      axis.ticks.x = element_blank(), 
      legend.position = "none")+
    coord_cartesian(xlim=range(yrs))
  p2=ggplot(adj)+
    geom_line(aes(year,data,group=iter,col=iter))+xlab("Year")+ylab("Change")+
    theme(legend.position="none")
  
  print(ggarrange(p1,p2,heights=c(4,1),ncol=1))
  }
  
  }  



#To incorporate a PID controller as a management procedure (harvest control rule) 
#into the `hcrICES` function for setting Total Allowable Catch (TAC) based on the 
#stock's status relative to a reference point for SSB, trends in an index of abundance, 
#and the variability, I'll modify the provided R code accordingly.

#This modification will include a simple PID controller logic to adjust the target 
#fishing mortality based on the deviation from the SSB reference point, incorporating 
#the proportional (P), integral (I), and derivative (D) components of the PID controller.

#First, define the PID parameters within the `hcrICES` function. Then, we'll 
#use these parameters to calculate the adjustment needed for the target fishing mortality 
#rate based on the PID controller logic.

#This documentation includes a description of the function, its parameters, the 
#return value, and an example usage scenario. Note that the example assumes the
#existence of `FLStock`, `FLBRP`, and other objects (`myStock`, `myBRP`, `myDeviances`), 
#which you'll need to define based on your specific data and context. The `FLR` package
#(`FLCore` and potentially others like `FLBRP`) should be installed and loaded in 
#your R environment to use these classes.

#Please adjust the example and parameter details as necessary to match the actual 
#implementation details and data structures in your project.

#' Harvest Control Rule with PID Controller Integration
#'
#' @description This function calculates the Total Allowable Catch (TAC) based on a modified hockey stick 
#' harvest control rule that incorporates a PID controller to adjust the target fishing mortality (F) 
#' according to the deviation of the current spawning stock biomass (SSB) from a reference point.
#' The PID controller aims to stabilize the SSB around the reference point by adjusting F, considering 
#' the current SSB level, its trend, and the variability in the index of abundance.
#'
#' @param object An `FLStock` object representing the fish stock under consideration.
#' @param eql An `FLBRP` object containing the stock-recruitment relationship used for projection.
#' @param sr_deviances An `FLQuant` object representing recruitment deviances on the log scale, i.e., multiplicative.
#' @param params An `FLPar` object containing the harvest control rule (HCR) parameters, specifying blim, btrig, bmin, ftar, and fmin.
#' @param start A numeric value indicating the first year for simulation.
#' @param end A numeric value indicating the last year for simulation.
#' @param interval A numeric value indicating the time step; defaults to 1 year.
#' @param lag A numeric value indicating the lag period; defaults to 1.
#' @param err An optional `FLQuant` object representing assessment error on SSB for the year used for HCR.
#' @param implErr A numeric value or `FLQuant` object representing implementation error; defaults to 0.
#' @param bndTac A numeric vector specifying bounds on TAC; defaults to c(0, Inf).
#' @param bndWhen A character string indicating when to apply bounds; defaults to "btrig".
#' @param bndCap A numeric value specifying the cap for TAC bounds; defaults to 1e6.
#' @param pidParams A list containing PID controller parameters: Kp, Ki, Kd, SSB_ref, and F_target.
#'
#' @return Returns a list containing an `FLStock` object with updated catch information and an `FLPar` object for the HCR.
#'
#' @examples
#' \dontrun{
#'   # Assuming 'myStock' is an FLStock object and 'myBRP' is an FLBRP object
#'   pidParams <- list(Kp=0.5, Ki=0.1, Kd=0.05, SSB_ref=2000, F_target=0.3)
#'   hcrParams <- FLPar(blim=0.7, btrig=1.0, bmin=0.5, ftar=0.3, fmin=0.1)
#'   
#'   # Run the hcrICES function with a PID controller
#'   result <- hcrICES(object=myStock, eql=myBRP, sr_deviances=myDeviances, params=hcrParams,
#'                     start=2010, end=2020, pidParams=pidParams)
#'   
#'   # Access the updated FLStock object
#'   updatedStock <- result[[1]]
#' }
#'
#' @export



#In this code snippet, I introduced a `pidAdjust` function within the 
#`hcrICES` method that calculates the PID adjustment based on the deviation of current 
#SSB from the reference SSB (`SSB_ref`). The PID parameters
#(`Kp`, `Ki`, `Kd`, `SSB_ref`, `F_target`) are passed to the 
#`hcrICES` function via the `pidParams` argument. This adjustment is then used to 
#modify the target fishing mortality rate (`F_target`), which in turn influences 
#the TAC setting.

#Please note that this implementation assumes access to the current SSB within the 
#`hcrICES` function, which would need to be obtained from the FLStock object or
#through other means. Also, the bounds for the adjusted target F (`F_target_adjusted`) 
#might need to be customized based on the specific fishery and management objectives.

#This code provides a starting point for integrating PID control logic into fisheries 
#management strategies within the R environment. Further refinement and testing would
#be necessary to ensure its applicability and effectiveness in specific management contexts.

