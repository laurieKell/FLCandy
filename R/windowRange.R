setMethod("window", signature(object="FLComp"),
          function(object,start=range(object)["minyear"], end=range(object)["maxyear"],
                     min  =range(object)["min"],     max=range(object)["max"],    plusgroup=range(object)["plusgroup"],   
                     extend=TRUE, frequency=1){
            
            object=qapply(object, window, start=start, end=end, extend=extend, frequency=frequency)
            
            object=object[as.numeric(dimnames(object)[[1]])>=min]
            
            if (!is.na(plusgroup)){
              object=setPlusGroup(object,plusgroup) 
            }else{
              object=object[as.numeric(dimnames(object)[[1]])<=max]}
        
        range(object)[c("minyear","maxyear","min","max","plusgroup")]=c(start,end,min,max,plusgroup)
            
        object})

if(FALSE){
data(ple4)
range(ple4)[c("minyear","maxyear","min","max","plusgroup","Minfbar")]=c(1980,2010,5,7,7,5)
plot(window(ple4))
}