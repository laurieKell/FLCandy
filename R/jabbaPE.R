## 1. Process error residuals ---------------------------------------------
getJabbaProcessError<-function(jb,
                                use = c("median", "mean"),
                                m.default = 2) {
  
  use = match.arg(use)
  
  ## Posterior parameter matrix (adapt if different)
  parpost = jb$pars_posterior
  
  get_par<-function(name) {
    if (!name %in% colnames(parpost))
      stop("Parameter ", name, " not found in jb$posteriors$pars.")
    if (use == "median") median(parpost[, name]) else mean(parpost[, name])
  }
  
  r =get_par("r")
  K =get_par("K")
  m =get_par("m")

  ts=jb$timeseries
  
  B    =ts[,1,"B"]
  C    =jb$est.catch[,"mu"]
  year =as.numeric(dimnames(ts)[[1]])
  
  n    = length(B)
  if (length(C) != n)
    stop("B and C lengths differ; check jb$timeseries structure.")
  
  ## Process error eps_t from state equation
  eps = mu = rep(NA_real_, n - 1)
  
  for (t in 1:(n - 1)) {
    Bt = B[t]
    Ct = C[t]
    
    prod = r * Bt * (1 - (Bt / K)^m)
    mu[t] = Bt + prod - Ct
    mu[t] = max(mu[t], 1e-12)
    
    eps[t] = log(B[t + 1] / mu[t])}
  
  data.frame(
    year   = years[1:(n - 1)],
    biomass= B[1:(n - 1)],
    bnext  = B[2:n],
    catch  = C[1:(n - 1)],
    mu     = mu,
    pe     = eps)}

## 2. Production function and realised trajectory 

getJabbaProduction = function(jb,
                              use       = c("median", "mean"),
                              m.default = 2,
                              BmaxK     = 1.5,
                              nGrid     = 200) {
  
  use = match.arg(use)
  
  ## Posterior parameter matrix (same as getJabbaProcessError)
  parpost = jb$pars_posterior
  
  get_par = function(name) {
    if (!name %in% colnames(parpost))
      stop("Parameter ", name, " not found in jb$posteriors$pars.")
    if (use == "median") median(parpost[, name]) else mean(parpost[, name])
  }
  
  r = get_par("r")
  K = get_par("K")
  m = get_par("m")  # or if you want a default: if ("m" %in% colnames(parpost)) get_par("m") else m.default
  
  ## Timeseries and catch extraction (same structure as getJabbaProcessError)
  ts   = jb$timeseries
  B    = ts[, 1, "B"]
  C    = jb$est.catch[, "mu"]
  yrs  = as.numeric(dimnames(ts)[[1]])
  n    = length(B)
  
  if (length(C) != n)
    stop("B and C lengths differ; check jb$timeseries / jb$est.catch structure.")
  
  ## 2a. Theoretical production curve P(B) = r B (1 - (B/K)^m)
  
  xRel  = seq(0.01, BmaxK, length.out = nGrid)   # B/K grid
  Bgrid = xRel * K
  Pgrid = r * Bgrid * (1 - (Bgrid / K)^m)
  
  prodFun = data.frame(
    biomass = Bgrid,
    bRel    = xRel,
    sp      = Pgrid)
  
  ## 2b. Realised surplus production trajectory SP_t = B_{t+1} - B_t + C_t
  
  SP = B[2:n] - B[1:(n - 1)] + C[1:(n - 1)]
  
  prodTraj = data.frame(
    year   = yrs[1:(n - 1)],
    biomass= B[1:(n - 1)],
    bRel   = B[1:(n - 1)] / K,
    prod   = SP)
  
  list(func = prodFun, traj = prodTraj)}


# jb <- your JABBA fit
pe=getJabbaProcessError(jb)
pf=getJabbaProduction(  jb)

ggplot()+
  geom_line(aes(biomass,sp),  data=pf$func)+
  geom_path(aes(biomass,prod),data=pf$traj)+
  geom_polygon(aes(year,data,group=regime),fill="blue",alpha=0.2,data=star)+


rodFn=FLife:::rodFn()

star=FLife:::rod(ssb(ple4))

ggplot(ssb(ple4))+
  geom_line( aes(year,data),linetype=3)+
  geom_point(aes(year,data))+
  geom_polygon(aes(year,data,group=regime),fill="blue",alpha=0.2,data=star)+
  theme_bw()+xlab("Year")+ylab("Residuals")