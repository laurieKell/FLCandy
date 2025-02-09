taxonomy<-function(x) {
  
  ldply(x, function(x){
    rtn=unlist(strsplit(x,"_")[[1]])
    names(rtn)=c("Subclass","order","family","genus","species")
    rtn})}

traits<-function(Taxon, 
                 params        =c("K", "M"), 
                 Database      =FishLife::FishBase_and_RAM,
                 SpeciesMatch  =NULL, 
                 partial_match =TRUE) {
  
  Cov_gjj       =Database$Cov_gvv 
  Mean_gj       =Database$beta_gv
  ParentChild_gz=Database$ParentChild_gz 
  Y_ij          =Database$Y_ij
  g_i           =Database$g_i 
  
  if (!all(params %in% colnames(Mean_gj))) return(NULL)
  
  if (partial_match) 
    Which <- grep(Taxon, ParentChild_gz[, "ChildName"])
  if (!partial_match) 
    Which <- which(Taxon == ParentChild_gz[, "ChildName"])
  if (length(Which) != 1) retunO(NULL)
  
  return(list(Cov_pred=Cov_gjj[Which, , ],
              Mean_pred=Mean_gj[Which, ]))}

taxa<-function(Taxa, 
                  params=matrix(c("K", "M", "Winfinity", "Loo", "tmax", "tm", 
                                  "Lm", "Temperature", "ln_margsd", "rho",
                                  "logitbound_h", "ln_r"), ncol=2, byrow=TRUE),
                  Database=FishLife::FishBase_and_RAM,
                  partial_match=FALSE) {
  
  Cov_gjj       =Database$Cov_gvv
  Mean_gj       =Database$beta_gv 
  ParentChild_gz=Database$ParentChild_gz
  Y_ij          =Database$Y_ij
  g_i           =Database$g_i 
  
  use_row=apply(params, MARGIN=1, function(charvec) {
    all(charvec %in% colnames(Mean_gj))})
  
  Pred=NULL
  for (rowI in 1:nrow(params)) {
    for (uniqueI in 1:length(unique(Taxa))) {
      Pred[[uniqueI]] <- suppressWarnings(
        traits(
          Taxon  = Taxa[uniqueI],
          params = params[rowI, ],
          partial_match = partial_match))}}
  return(Pred)}


priors<-function(Genus  =NULL,
                 Species=NULL,
                 Family =NULL,
                 params =c("K", "M", "Winfinity", "Loo", "tmax", "tm", 
                           "Lm", "Temperature", "ln_margsd", "rho",
                           "logitbound_h", "ln_r")) {
  match=NULL
  
  if (!is.null(Species)&!is.null(Genus))
    match=tryCatch(suppressWarnings(Search_species(Genus=Genus,Species=Species)),
      error = function(e) return(NULL))
  
  if ((is.null(match))|(is.null(Species)&!is.null(Genus)))
    match=tryCatch(suppressWarnings(Search_species(Genus=Genus)),
      error = function(e)  return(NULL))

  if ((is.null(match))|(is.null(Species)&is.null(Genus)&!is.null(Family)))
    match=tryCatch(suppressWarnings(Search_species(Family=Family)),
      error = function(e) return(NULL))
  
  if(is.null(match)) return(NULL)
  
  rtn=tryCatch(taxa(match$match_taxonomy,params=matrix(params,ncol=2, byrow=TRUE)),
       error = function(e) return(NULL))
  
  if (inherits(rtn, "try-error")) return(NULL)
  
  rtn=cbind(taxonomy(match$match_taxonomy),
            ldply(rtn, function(x) x$Mean_pred),
            var=ldply(rtn, function(x) diag(x$Cov_pred)))
  
  predictive=rtn[,c("Subclass","order","family","genus","species")]
  predictive=predictive=="predictive"
  predictive=order(apply(predictive,1,sum))+5-dim(rtn)[1]
  predictive=rev(c("Subclass","order","family","genus","species"))[predictive]
  
  rtn=cbind(predictive=predictive,rtn[,-c(1:5)])
  
  return(rtn)}
  
