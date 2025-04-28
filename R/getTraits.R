getTraits<-function(Class        ="predictive", 
                    Order        ="predictive", 
                    Family       ="predictive", 
                    Genus        ="predictive", 
                    Species      ="predictive",
                    partial_match=!TRUE,
                    add_ancestors=!TRUE, 
                    Database     =FishLife::FishBase_and_RAM, 
                    ParentChild_gz=Database$ParentChild_gz ){
  
  taxa=Search_species(Class,Order,Family,Genus,Species,add_ancestors,Database,ParentChild_gz)$match_taxonomy
  
  if(partial_match==TRUE)  Which=grep( taxa, ParentChild_gz[,'ChildName'])
  if(partial_match==FALSE) Which=which(taxa==ParentChild_gz[,'ChildName'])
  if( length(Which)!=1 ) stop( paste0("'Taxon' ",taxa," input matches more or less than one element") )
  
  Cov_gjj       =Database$Cov_gvv
  Mean_gj       =Database$beta_gv
  ParentChild_gz=Database$ParentChild_gz
  Y_ij          =Database$Y_ij
  g_i           =Database$g_i
  
  nms      =c("linf","k","winf","amax","amat","m","l50","temperature","lnvar","rho",
              "lnmasps","lnmargsd","s","logits","fmsym","fmsy","lnr","r","lhg","g")
  mu       =Mean_gj[Which,]
  names(mu)=nms
  mu       =FLPar(mu)
  
  cov      =Cov_gjj[Which,,]
  cov      =as(array(cov,c(dim(cov),1),dimnames=list(params=nms,params=nms,iter=1)),"FLPar")
  
  FLPars(mu=mu,cov=cov)}

shapeFn<-function(r,fmsy){
  m=r/fmsy
  shape=m^-(1/(m-1))
  shape}

