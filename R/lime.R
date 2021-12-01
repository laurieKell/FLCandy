lime_lh<-function(x,selex_type="flat",CVlen=0.2,m=NULL){
  
  create_lh_list(
    #list(
    # growth
    linf  =unlist(c(x["linf"])),
    vbk   =unlist(c(x["k"])),
    t0    =unlist(c(x["t0"])),
    lwa   =unlist(c(x["a"])),
    lwb   =unlist(c(x["b"])),
    AgeMax=unlist(c(vonB(params=x,length=x["linf"]*0.9))),
    
    # M
    M   =ifelse(is.null(m), mean(gislason(FLQuant(2:12),as(x,"FLPar"))),m),
    
    # mat
    maturity_input="length",
    M50  =unlist(c(x["l50"])),
    
    # SRR
    h  =unlist(c(x["s"])),
    
    # selex
    selex_input="length",
    selex_type =selex_type,
    S50    =unlist(c(x["a50"])),
    S95    =unlist(c(x["a50"]+x["ato95"])),
    dome_sd=2,
    #
    CVlen=CVlen)}

lens<-function(x){
  fpp=cast(x,year~length,value="data")
  fpp=ddply(fpp, .(year), function(x) {x[is.na(x)]=0;x})
  
  fp=as.matrix(fpp[,-1],rownames=T)
  names(dimnames(fp))=c("year","length")
  dimnames(fp)$year=fpp[,1]
  fp}

runLime<-function(lfd,lh){

  lh =lime_lh(lh)
  
  res=foreach(iter=dimnames(lfd)$iter, 
        .combine=rbind,
        .multicombine=TRUE,
        .export=c("lh","lfd"),
        .packages=c("LIME","FLCore","data.table","FLife","plyr")) %dopar% {
                
    ln    =matrix(iter(lfd,iter),dim(lfd)[2],dim(lfd)[1],dimnames=dimnames(lfd)[2:1])
    inputs=create_inputs(lh=lh, list(years=dimnames(ln)$year, LF=ln))
                
    res=try(run_LIME("/home/laurence-kell/Desktop/tmp", 
               input=inputs, data_avail=c("LC"), 
               vals_selex_ft=inputs$S_fl, 
               est_selex_f=FALSE, 
               C_type=0,
               derive_quants=TRUE,
               newtonsteps=FALSE)[c("Report","Derived")])
                
        if (!("try-error"%in%is(res))){
            data.frame("iter" =iter,
                       "year" =dimnames(ln)$year,
                       "F"    =res[["Report"]]$F_y,
                       "FFmsy"=res[["Derived"]]$FFmsy)
             }
        }
  res}

