load("/home/laurie/Downloads/Updated_stks_n81.rdata")

stksel = c("pil.27.8c9a",
           "pra.27.3a4a",
           "her.27.3a47d",
           "ple.27.420",
           "cod.27.6a",
           "mon.27.78abd",
           "mac.27.nea",
           "whb.27.1-91214",
           "bss.27.8ab")[-2]

stks=stks[stksel]

dat=ldply(stks, function(x) as.data.frame(FLQuants(x, 
            mnAge   =function(x) apply(catch.n(x)%*%ages(catch.n(x)),2,sum)%/%apply(catch.n(x),2,sum),
            catch   =catch,
            f       =fbar,
            vBiomass=vb),drop=T))
dat=ddply(dat, .(.id,qname), with, data.frame(Year=year,data=data/mean(data)))

trendFn<-function(year,data,n=3){
     mdply(n:length(year), function(x) 
       coefficients(lm(data[x-2:0]~year[x-2:0]))[[2]])}
  
trnds=ddply(dat, .(.id,qname), with, {
  res=try(trendFn(year,data))
  if ("try-error"%in%is(res)) return(NULL) else return(res)})
trnds=cast(trnds,.id+X1~qname,value="V1")

my_smooth <- function(data,mapping,...){
  ggplot(data=data,mapping=mapping)+
    geom_point(...,size=.5)+
    geom_smooth(...,method="lm",se=FALSE)}

ggpairs(trnds[,3:6],
        lower = list(continuous = wrap(my_smooth)))

     
dat=ldply(stks, function(x) model.frame(FLQuants(x, 
                                                   mnAge   =function(x) apply(catch.n(x)%*%ages(catch.n(x)),2,sum)%/%apply(catch.n(x),2,sum),
                                                   catch   =catch,
                                                   f       =fbar,
                                                   vBiomass=vb),drop=T))

ccf=ddply(dat, .(.id), with, ccf(f,1/mnAge,lag.max=5,na.action=na.pass,plot=FALSE)$acf)
ccf=transform(melt(ccf),lag=an(variable)-6)





