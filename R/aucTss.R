aucTss<-function(om,mp){
  rtn=mydas:::roc(om,mp)

  data.frame("tss"=data.frame(with(rtn[abs(rtn$reference-1)==min(abs(rtn$reference-1)),],TPR-FPR))[1,],
  data.frame("auc"=pROC:::auc(as.character(rtn$label),rtn$reference)))}

