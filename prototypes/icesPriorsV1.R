require(JABBA)
require(FLCore)
require(FLBRP)
require(kobe)

require(ggplot2)
require(ggpubr)
require(ggplotFL)

require(plyr)
require(dplyr) 

load("P:/rfmo/ices/data/Updated_stks_n81_R0_updated2023.RData")

# Create a data frame with ICES stock codes and corresponding species details
stock=c("ank.27.78abd", "aru.27.5a14", "aru.27.5b6a", "bss.27.4bc7ad-h", "bss.27.8ab", 
        "cod.21.1", "cod.2127.1f14", "cod.27.1-2", "cod.27.1-2coastN", "cod.27.22-24", 
        "cod.27.24-32", "cod.27.47d20", "cod.27.5a", "cod.27.5b1", "cod.27.6a", 
        "cod.27.7a", "cod.27.7e-k", "had.27.1-2", "had.27.46a20", "had.27.5a", 
        "had.27.5b", "had.27.6b", "had.27.7a", "had.27.7b-k", "her.27.1-24a514a", 
        "her.27.20-24", "her.27.25-2932", "her.27.28", "her.27.3031", "her.27.3a47d", 
        "her.27.5a", "her.27.6a7bc", "her.27.irls", "her.27.nirs", "hke.27.3a46-8abd", 
        "hke.27.8c9a", "hom.27.2a4a5b6a7a-ce-k8", "hom.27.9a", "ldb.27.8c9a", "lin.27.5a", 
        "lin.27.5b", "mac.27.nea", "meg.27.7b-k8abd", "meg.27.8c9a", "mon.27.78abd", 
        "mon.27.8c9a", "pil.27.8abd", "pil.27.8c9a", "ple.27.21-23", "ple.27.420", 
        "ple.27.7a", "ple.27.7d", "pok.27.1-2", "pok.27.3a46", "pok.27.5a", 
        "pok.27.5b", "pra.27.3a4a", "reb.27.1-2", "reg.27.1-2", "reg.27.561214", 
        "san.sa.1r", "san.sa.2r", "san.sa.3r", "san.sa.4", "sol.27.20-24", 
        "sol.27.4", "sol.27.7a", "sol.27.7d", "sol.27.7e", "sol.27.7fg", 
        "sol.27.8ab", "spr.27.22-32", "spr.27.3a4", "tur.27.4", "usk.27.5a14", 
        "whb.27.1-91214", "whg.27.47d", "whg.27.6a", "whg.27.7a", "whg.27.7b-ce-k", 
        "wit.27.3a47d")

name=c("Anglerfish", "Atlantic redfishes", "Atlantic redfishes", "European seabass", 
       "European seabass", "Atlantic cod", "Atlantic cod", "Atlantic cod", 
       "Atlantic cod", "Atlantic cod", "Atlantic cod", "Atlantic cod", 
       "Atlantic cod", "Atlantic cod", "Atlantic cod", "Atlantic cod", 
       "Atlantic cod", "Haddock", "Haddock", "Haddock", 
       "Haddock", "Haddock", "Haddock", "Haddock", "Atlantic herring", 
       "Atlantic herring", "Atlantic herring", "Atlantic herring", 
       "Atlantic herring", "Atlantic herring", "Atlantic herring", 
       "Atlantic herring", "Atlantic herring", "Atlantic herring", 
       "European hake", "European hake", "Atlantic horse mackerel", 
       "Atlantic horse mackerel", "Four-spot megrim", "Ling", 
       "Ling", "Mackerel", "Megrim", "Megrim", "Anglerfish", 
       "Anglerfish", "European pilchard", "European pilchard", 
       "European plaice", "European plaice", "European plaice", 
       "European plaice", "Saithe", "Saithe", "Saithe", 
       "Saithe", "Northern prawn", "Redfish", "Redfish", "Redfish", 
       "Sandeel", "Sandeel", "Sandeel", "Sandeel", "Sole", 
       "Sole", "Sole", "Sole", "Sole", "Sole", 
       "Sole", "Sprat", "Sprat", "Turbot", "Tusk", 
       "Blue whiting", "Whiting", "Whiting", "Whiting", "Whiting", 
       "Witch flounder")

spp=c("Lophius piscatorius", "Sebastes spp.", "Sebastes spp.", "Dicentrarchus labrax", 
      "Dicentrarchus labrax", "Gadus morhua", "Gadus morhua", "Gadus morhua", 
      "Gadus morhua", "Gadus morhua", "Gadus morhua", "Gadus morhua", 
      "Gadus morhua", "Gadus morhua", "Gadus morhua", "Gadus morhua", 
      "Gadus morhua", "Melanogrammus aeglefinus", "Melanogrammus aeglefinus", "Melanogrammus aeglefinus", 
      "Melanogrammus aeglefinus", "Melanogrammus aeglefinus", "Melanogrammus aeglefinus", "Melanogrammus aeglefinus", "Clupea harengus", 
      "Clupea harengus", "Clupea harengus", "Clupea harengus", 
      "Clupea harengus", "Clupea harengus", "Clupea harengus", 
      "Clupea harengus", "Clupea harengus", "Clupea harengus", 
      "Merluccius merluccius", "Merluccius merluccius", "Trachurus trachurus", 
      "Trachurus trachurus", "Lepidorhombus boscii", "Molva molva", 
      "Molva molva", "Scomber scombrus", "Lepidorhombus whiffiagonis", 
      "Lepidorhombus whiffiagonis", "Lophius piscatorius", 
      "Lophius piscatorius", "Sardina pilchardus", "Sardina pilchardus", 
      "Pleuronectes platessa", "Pleuronectes platessa", "Pleuronectes platessa", 
      "Pleuronectes platessa", "Pollachius virens", "Pollachius virens", "Pollachius virens", 
      "Pollachius virens", "Pandalus borealis", "Sebastes spp.", "Sebastes spp.", "Sebastes spp.", 
      "Ammodytes spp.", "Ammodytes spp.", "Ammodytes spp.", "Ammodytes spp.", "Solea solea", 
      "Solea solea", "Solea solea", "Solea solea", "Solea solea", 
      "Solea solea", "Solea solea", "Sprattus sprattus", 
      "Sprattus sprattus", "Scophthalmus maximus", "Brosme brosme", 
      "Micromesistius poutassou", "Merlangius merlangus", "Merlangius merlangus", "Merlangius merlangus", 
      "Merlangius merlangus", "Glyptocephalus cynoglossus")
Genus=c("Lophius", "Sebastes", "Sebastes", "Dicentrarchus", 
        "Dicentrarchus", "Gadus", "Gadus", "Gadus", 
        "Gadus", "Gadus", "Gadus", "Gadus", 
        "Gadus", "Gadus", "Gadus", "Gadus", 
        "Gadus", "Melanogrammus", "Melanogrammus", "Melanogrammus", 
        "Melanogrammus", "Melanogrammus", "Melanogrammus", "Melanogrammus", "Clupea", 
        "Clupea", "Clupea", "Clupea", 
        "Clupea", "Clupea", "Clupea", 
        "Clupea", "Clupea", "Clupea", 
        "Merluccius", "Merluccius", "Trachurus", 
        "Trachurus", "Lepidorhombus", "Molva", 
        "Molva", "Scomber", "Lepidorhombus", 
        "Lepidorhombus", "Lophius", 
        "Lophius", "Sardina", "Sardina", 
        "Pleuronectes", "Pleuronectes", "Pleuronectes", 
        "Pleuronectes", "Pollachius", "Pollachius", "Pollachius", 
        "Pollachius", "Pandalus", "Sebastes", "Sebastes", "Sebastes", 
        "Ammodytes", "Ammodytes", "Ammodytes", "Ammodytes", "Solea", 
        "Solea", "Solea", "Solea", "Solea", 
        "Solea", "Solea", "Sprattus", 
        "Sprattus", "Scophthalmus", "Brosme", 
        "Micromesistius", "Merlangius", "Merlangius", "Merlangius", 
        "Merlangius", "Glyptocephalus")
spp=data.frame(stock=stock,name=name,spp=spp,Genus=Genus,Species=unlist(strsplit(spp," "))[seq(2,length(spp)*2,2)])


ctc=read.csv("C:/active/haf/framework/data/NorthSea_stocks.csv")
ctc=subset(ctc,!is.na(Catches))


# Create the data frame
lw=data.frame(
    Common_Name = c(
      "Anglerfish", 
      "Atlantic Redfishes", 
      "European Seabass", 
      "Atlantic Cod", 
      "Haddock", 
      "Atlantic Herring", 
      "European Hake", 
      "Atlantic Horse Mackerel", 
      "Four-spot Megrim", 
      "Ling", 
      "Mackerel", 
      "Megrim", 
      "European Pilchard", 
      "European Plaice", 
      "Saithe", 
      "Northern Prawn", 
      "Redfish", 
      "Sandeel", 
      "Sole", 
      "Sprat", 
      "Turbot", 
      "Tusk", 
      "Blue Whiting", 
      "Whiting", 
      "Witch Flounder"
    ),
    Latin_Name = c(
      "Lophius piscatorius", 
      "Sebastes spp.", 
      "Dicentrarchus labrax", 
      "Gadus morhua", 
      "Melanogrammus aeglefinus", 
      "Clupea harengus", 
      "Merluccius merluccius", 
      "Trachurus trachurus", 
      "Lepidorhombus boscii", 
      "Molva molva", 
      "Scomber scombrus", 
      "Lepidorhombus whiffiagonis", 
      "Sardina pilchardus", 
      "Pleuronectes platessa", 
      "Pollachius virens", 
      "Pandalus borealis", 
      "Sebastes marinus", 
      "Ammodytes spp.", 
      "Solea solea", 
      "Sprattus sprattus", 
      "Scophthalmus maximus", 
      "Brosme brosme", 
      "Micromesistius poutassou", 
      "Merlangius merlangus", 
      "Glyptocephalus cynoglossus"
    ),
    a = c(
      0.0045, 
      0.0050, 
      0.0075, 
      0.0061, 
      0.0053, 
      0.0032, 
      0.0048, 
      0.0059, 
      0.0046, 
      0.0038, 
      0.0057, 
      0.0046, 
      0.0059, 
      0.0038, 
      0.0062, 
      0.0015, 
      0.0050, 
      0.0055, 
      0.0042, 
      0.0050, 
      0.0071, 
      0.0060, 
      0.0052, 
      0.0054, 
      0.0043
    ),
    b = c(
      3.25, 
      3.10, 
      3.10, 
      3.04, 
      3.15, 
      3.24, 
      3.12, 
      3.08, 
      3.20, 
      3.25, 
      3.10, 
      3.20, 
      3.02, 
      3.20, 
      3.15, 
      2.98, 
      3.10, 
      3.05, 
      3.12, 
      3.10, 
      3.15, 
      3.18, 
      3.10, 
      3.15, 
      3.20
    ),
    Source = c(
      "FishBase", 
      "FishBase", 
      "ResearchGate", 
      "FishBase", 
      "ScienceDirect", 
      "ResearchGate", 
      "ResearchGate", 
      "ResearchGate", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "ResearchGate", 
      "ScienceDirect", 
      "FishBase", 
      "NAFO", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "FishBase", 
      "FishBase"
    )
  )
  
  

priorFn<-function(x,nmin=0:2,nmax=0:2){
  .
  fmsy     =unlist(c(attributes(x)$benchmark["Fmsy"]))
  bmsy     =unlist(c(attributes(x)$eqsim["BMSY"]))
  b0       =unlist(c(attributes(x)$eqsim["B0"]))
  
  ssb.minyr=mean(ssb( x)[,1+nmin])
    f.minyr=mean(fbar(x)[,1+nmin])
    h.minyr=mean(hr(  x)[,1+nmin])
  ssb.maxyr=mean(ssb( x)[,dim(ssb( x))[2]-nmax])
    f.maxyr=mean(fbar(x)[,dim(fbar(x))[2]-nmax])
    h.maxyr=mean(hr(  x)[,dim(fbar(x))[2]-nmax])
  
  shape=bmsy/b0
  
  if (is.na(shape)) return(NULL)
  
  mi   =seq(0.01,2,0.001) 
  m    =(mi^(-1/(mi-1))-shape)^2
  m    =mi[m==min(m)]
  
  r    =(1-exp(-fmsy))*(m-1)/(1-m^-1)
  
  rtn=c(r=r,mpar=m,fmsy=fmsy,bmsy=bmsy,b0=b0,
        ssb.minyr=ssb.minyr,ssb.maxyr=ssb.maxyr,
          f.minyr=  f.minyr,  f.maxyr=  f.maxyr,
          h.minyr=  h.minyr,  h.maxyr=  h.maxyr)
  names(rtn)=c("r","mpar","fmsy","bmsy","b0",
               "ssb.minyr","ssb.maxyr",
                 "f.minyr",  "f.maxyr",
                 "h.minyr",  "h.maxyr")
  
  if (is.na(rtn["r"])) return(NULL)
  rtn}

ebiomass<-function(object){
  sel  =harvest(object)
  wt   =catch.wt(object)%*%sel%/%fapex(sel)
  eb.wt=qmax(wt,0.000001)
  
  apply(eb.wt%*%stock.n(object),2:6,sum)}

dataFn<-function(object){
  
  model.frame(FLQuants(catch   =catch(object),
                       eb      =ebiomass(object),
                       h       =ebiomass(object)/catch(object),
                       ssb     =ssb(object),
                       f       =fbar(object),
                       m       =FLQuant(aaply(m(object)[ac(range(object)["minfbar"]:range(object)["maxfbar"])],2,mean),
                                        dimnames=dimnames(fbar(object)))),drop=TRUE)
  
  }

ldfFn<-function(object){
  
  ak =invALK(iter(par,1),cv=0.1,age=an(dimnames(object)$age),bin=1)
  lfd=lenSamples(catch.n(object),ak,n=5000)
  
  units(lfd)="cm"
  
  lfd}

priors =ldply(ICESStocks, priorFn)
tseries=ldply(ICESStocks, dataFn)

p1=ggplot(transmute(priors,ssb=ssb.maxyr/bmsy))+
  geom_histogram(aes(x = ssb,  y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(aes(x = ssb), fill = "white", alpha = 0.5) +
  labs(title = "Current for All ICES Stocks",
       x = "SSB/BMSY",
       y = "Density") + theme_minimal()
  
p2=ggplot(transmute(priors,f=f.maxyr/fmsy))+
  geom_histogram(aes(x=f,  y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(  aes(x=f), fill = "white", alpha = 0.5) +
  scale_x_log10()+
  labs(title = "Current for All ICES Stocks",
       x = "F/FMSY",
       y = "Density") + theme_minimal()
ggarrange(p1,p2,ncol=1)


# Decompose each time series to separate the trend, seasonal, and irregular 
# components. This helps in identifying the process erro
data_ts <- ices %>%
  mutate(year = as.integer(year)) %>%
  as_tsibble(index = year, key = .id)

# Decompose the time series for each .id
decomposed_ts <- data_ts %>%
  group_by_key() %>%
  model(STL(ssb ~ trend() + season()))

# Extract the components
components <- decomposed_ts %>%
  components() %>%
  as_tibble()

# Fit an ARIMA model for each .id
fit <- data_ts %>%
  group_by_key() %>%
  model(ARIMA(ssb))

# Summarize the models
fit_summary <- fit %>%
  glance() %>%
  as_tibble()

# Calculate residuals
residuals <- fit %>%
  residuals() %>%
  as_tibble()

# Summarize the residuals
residuals_summary <- residuals %>%
  group_by(.id) %>%
  summarize(
    mae = mean(abs(.resid)),
    mse = mean(.resid^2),
    rmse = sqrt(mse)
  )

# Histogram of residuals
ggplot(residuals, aes(x = .resid)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  facet_wrap(~ .id, scales = "free")

# Residuals over time
ggplot(residuals, aes(x = year, y = .resid)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Residuals Over Time", x = "Time", y = "Residuals") +
  facet_wrap(~ .id, scales = "free")

# Print error metrics
print(residuals_summary)

# Convert to tibble for anomalize
data_tbl <- as_tibble(data_ts)

# Anomaly detection
anomalized_data <- data_tbl %>%
  time_decompose(ssb, method = "stl") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose()

# Plot anomalies
plot_anomalies(anomalized_data)


jabs=mlply(unique(priors$.id), function(stock){
    print(stock)
    set.seed(123)
    
    catch=subset(tseries,.id==stock)[,c("year","catch")]
    index=subset(tseries,.id==stock)[,c("year","eb")]
    index$index = index$eb/mean(index$eb,na.rm=T)
    index$index[1:(length(index$index)-9)]=NA
    
    prior=subset(priors,.id==stock)
    r    =unlist(c(prior[,c("r")]))
    psi  =unlist(c(prior[,c("ssb.minyr")]/prior[,c("b0")]))
    
    # Fit with Catch + Index: Simple Fox
    jbI=try(build_jabba(catch      =catch,
                        cpue       =index,
                        model.type ="Fox",
                        scenario   =stock,
                        r.prior    =c(r,0.3),
                        verbose    =F,
                        psi.prior=c(psi,0.3)))
    
    if ("try-error"%in%is(jbI)) return(NULL)
    
    jbI=try(fit_jabba(jbI,quickmcmc = T,verbose=F))
    
    if ("try-error"%in%is(jbI)) return(NULL)
    
    jbI})

jbplot_summary(jabs[1])

res=ldply(jabs10, function(x) cbind(.id =x$scenario,
                                  year=dimnames(x$timeseries)[[1]],
                                  as.data.frame(x$timeseries[,1,])))[,-1]
names(res)=c(".id","year","eb","h","bbmsy","ffmsy","bb0","pe","sp")

ts =merge(tseries,priors,by=".id")
names(res)[-(1:2)]=paste("jabba",names(res)[-(1:2)],sep=".")
ts =merge(ts,res,by=c(".id","year"))
save(jabs,ts,file="C:/active/haf/framework/data/jabs.RData")

ggplot(ddply(ts,.(.id), with, 
             data.frame(Year=year,
                        OM  =ssb/bmsy/(mean(ssb/bmsy)),
                        MP  =jabba.bb0/(mean(jabba.bb0)))))+
  geom_line(aes(Year,OM))+
  geom_line(aes(Year,MP),col="red")+
  facet_wrap(~.id,scale="free")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.y  = element_blank())+
  ylab("")


ggplot(ts)+
  geom_line(aes(year,jabba.pe))+
  facet_wrap(~.id,scale="free")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.y  = element_blank())+
  ylab("")

p1=gghistogram(ddply(ts, .(.id), with, pROC::auc(ssb/median(ssb)>1,jabba.bb0)),x="V1")+xlab("AUC")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.y  = element_blank())+
  ylab("")
p2=ggplot(ddply(ts, .(.id), with, FLCandy:::roc(ssb/median(ssb),jabba.bb0)))+ 
  geom_line(aes(FPR,TPR,group=.id))+
  geom_abline(aes(intercept=0,slope=1),col="red")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.y  = element_blank())+
  ylab("")

ggarrange(p1,p2,ncol=1,heights=c(1,2),labels=c("AUC","ROC"))


jabz=mlply(unique(priors$.id), function(stock){
  print(stock)
  set.seed(123)
  
  catch      =subset(ts,.id==stock)[,c("year","Catch")]
  index      =subset(ts,.id==stock)[,c("year","EBiomass")]
  index$index=index$EBiomass/mean(index$EBiomass,na.rm=T)
  z          =subset(ts,.id==stock)[,c("year","FFmsy")]
  
  prior=subset(priors,.id==stock)
  r    =unlist(c(prior[,c("r")]))
  psi  =unlist(c(prior[,c("ssb.minyr")]/prior[,c("b0")]))
  
  # Fit with Catch + Index: Simple Fox with r = Fmsy
  jbI=try(build_jabba(catch      =catch,
                      cpue       =NULL,
                      auxiliary  =z,
                      auxiliary.sigma = TRUE, # Here estimated
                      auxiliary.obsE =0.3, # 
                      auxiliary.lag  =0, # lag effect between impact and Z pop structure
                      auxiliary.type="ffmsy",
                      model.type ="Fox",
                      scenario   =stock,
                      r.prior    = c(r,0.3),
                      verbose    =F,
                      psi.prior=c(psi,0.3)))
  
  if ("try-error"%in%is(jbI)) return(NULL)
  
  jbI=try(fit_jabba(jbI,quickmcmc = T,verbose=F))
  
  if ("try-error"%in%is(jbI)) return(NULL)
  
  jbI})

jbplot_summary(jabz[1])

res=ldply(jabz, function(x) cbind(.id =x$scenario,
                                  year=dimnames(x$timeseries)[[1]],
                                  as.data.frame(x$timeseries[,1,])))[,-1]
ts =merge(tseries,priors,by=".id")
ts =merge(ts,res,by=c(".id","year"))
ts=ddply(ts, .(.id), transform, ssb.=ssb/median(ssb))
save(jabz,ts,file="C:/active/haf/framework/data/jabz.RData")

rtn=ddply(ts, .(.id), with, roc(ssb.,BB0))
ggplot(rtn)+ 
  geom_line(aes(FPR,TPR,group=.id))+
  geom_abline(aes(intercept=0,slope=1),col="red")
ddply(ts, .(.id), with, auc(ssb.>1,BB0))

gghistogram(ddply(ts, .(.id), with, auc(ssb.>1,BB0)),x="V1")+xlab("AUC")

jabz2=mlply(unique(priors$.id), function(stock){
  print(stock)
  set.seed(123)
  
  catch      =subset(ts,.id==stock)[,c("year","Catch")]
  index      =subset(ts,.id==stock)[,c("year","EBiomass")]
  index$index=index$EBiomass/mean(index$EBiomass,na.rm=T)
  z          =subset(ts,.id==stock)[,c("year","FFmsy")]
  z[-c(1:3,dim(z)[1]-(0:2)),2]=NA
  
  prior=subset(priors,.id==stock)
  r    =unlist(c(prior[,c("r")]))
  psi  =unlist(c(prior[,c("ssb.minyr")]/prior[,c("b0")]))
  
  # Fit with Catch + Index: Simple Fox with r = Fmsy
  jbI=try(build_jabba(catch      =catch,
                      cpue       =NULL,
                      auxiliary  =z,
                      auxiliary.sigma = TRUE, # Here estimated
                      auxiliary.obsE =0.3, # 
                      auxiliary.lag  =0, # lag effect between impact and Z pop structure
                      auxiliary.type="ffmsy",
                      model.type ="Fox",
                      scenario   =stock,
                      r.prior    = c(r,0.3),
                      verbose    =F,
                      psi.prior=c(psi,0.3)))
  
  if ("try-error"%in%is(jbI)) return(NULL)
  
  jbI=try(fit_jabba(jbI,quickmcmc = T,verbose=F))
  
  if ("try-error"%in%is(jbI)) return(NULL)
  
  jbI})

jbplot_summary(jabz2[1])

res=ldply(jabz2, function(x) cbind(.id =x$scenario,
                                  year=dimnames(x$timeseries)[[1]],
                                  as.data.frame(x$timeseries[,1,])))[,-1]
ts =merge(tseries,priors,by=".id")
ts =merge(ts,res,by=c(".id","year"))
ts=ddply(ts, .(.id), transform, ssb.=ssb/median(ssb))
save(jabz2,ts,file="C:/active/haf/framework/data/jabz2.RData")

rtn=ddply(ts, .(.id), with, roc(ssb.,BB0))
ggplot(rtn)+ 
  geom_line(aes(FPR,TPR,group=.id))+
  geom_abline(aes(intercept=0,slope=1),col="red")
ddply(ts, .(.id), with, auc(ssb.>1,BB0))

gghistogram(ddply(ts, .(.id), with, auc(ssb.<1,BB0)),x="V1")+xlab("AUC")



jabz4=mlply(unique(priors$.id), function(stock){
  print(stock)
  set.seed(123)
  
  catch      =subset(ts,.id==stock)[,c("year","Catch")]
  index      =subset(ts,.id==stock)[,c("year","EBiomass")]
  index$index=index$EBiomass/mean(index$EBiomass,na.rm=T)
  z          =subset(ts,.id==stock)[,c("year","Z")]
  z[,2]      =1-exp(-z[,2])
  
  prior=subset(priors,.id==stock)
  r    =unlist(c(prior[,c("r")]))*2
  psi  =unlist(c(prior[,c("ssb.minyr")]/prior[,c("b0")]))
  
  # Fit with Catch + Index: Simple Fox with r = Fmsy
  jbI=try(build_jabba(catch      =catch,
                      cpue       =NULL,
                      auxiliary  =z,
                      auxiliary.sigma = TRUE, # Here estimated
                      auxiliary.obsE =0.3, # 
                      auxiliary.lag  =0, # lag effect between impact and Z pop structure
                      auxiliary.type="z",
                      model.type ="Fox",
                      scenario   =stock,
                      r.prior    = c(r,0.3),
                      verbose    =F,
                      psi.prior=c(psi,0.3)))
  
  if ("try-error"%in%is(jbI)) return(NULL)
  
  jbI=try(fit_jabba(jbI,quickmcmc = T,verbose=F))
  
  if ("try-error"%in%is(jbI)) return(NULL)
  
  jbI})

jbplot_summary(jabz3[1])

res=ldply(jabz3, function(x) cbind(.id =x$scenario,
                                   year=dimnames(x$timeseries)[[1]],
                                   as.data.frame(x$timeseries[,1,])))[,-1]
ts =merge(tseries,priors,by=".id")
ts =merge(ts,res,by=c(".id","year"))
ts=ddply(ts, .(.id), transform, ssb.=ssb/median(ssb))
save(jabz3,ts,file="C:/active/haf/framework/data/jabz2.RData")

ggplot(ddply(ts,.(.id), with, 
             data.frame(Year=year,
                        OM  =ssb/bmsy/(mean(ssb/bmsy)),
                        MP  =BB0/(mean(BB0)))))+
  geom_line(aes(Year,OM))+
  geom_line(aes(Year,MP),col="red")+
  facet_wrap(~.id,scale="free")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.y  = element_blank())+
  ylab("")

rtn=ddply(ts, .(.id), with, roc(ssb.,BB0))
ggplot(rtn)+ 
  geom_line(aes(FPR,TPR,group=.id))+
  geom_abline(aes(intercept=0,slope=1),col="red")
ddply(ts, .(.id), with, auc(ssb.>1,BB0))

gghistogram(ddply(ts, .(.id), with, auc(ssb.>1,BB0)),x="V1")+xlab("AUC")

load("C:/active/haf/framework/data/jabz.RData")
ggplot(ddply(ts,.(.id), with, 
             data.frame(Year=year,
                        OM  =ssb/bmsy/(mean(ssb/bmsy)),
                        MP  =BB0/(mean(BB0)))))+
  geom_line(aes(Year,OM))+
  geom_line(aes(Year,MP),col="red")+
  facet_wrap(~.id,scale="free")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.y  = element_blank())+
  ylab("")

load("C:/active/haf/framework/data/jabs.RData")
ggplot(ddply(ts,.(.id), with, 
             data.frame(Year=year,
                        OM  =ssb/bmsy/(mean(ssb/bmsy)),
                        MP  =BB0/(mean(BB0)))))+
  geom_line(aes(Year,OM))+
  geom_line(aes(Year,MP),col="red")+
  facet_wrap(~.id,scale="free")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.y  = element_blank())+
  ylab("")


ggplot(ddply(ctc,.(FishStock), transform, ctc=Catches/mean(Catches,na.rm=T)))+
  geom_line(aes(Year,ctc,line=FishStock))

