cunit<-function(...){
  args<-list(...)
  
  rtn=expand(args[[1]],unit=seq(length(args)))

  for (i in args[-1])
    rtn[,,i]=args[[i]]
  
  rtn}

# do.call('FLStock', Map(function(i,j) ubind(i,j), 
#                        i=as(ple4, 'FLQuants'), 
#                        j=as(ple4, 'FLQuants')))

