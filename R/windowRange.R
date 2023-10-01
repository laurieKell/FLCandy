setMethod("window", signature(x="FLComp"),
          function(x,start=range(x)["minyear"], end=range(x)["maxyear"],
                     min  =range(x)["min"],     max=range(x)["max"],    plusgroup=range(x)["plusgroup"],   
                     extend=TRUE, frequency=1){
            
            x=qapply(x, window, start=start, end=end, extend=extend, frequency=frequency)
            
            x=x[as.numeric(dimnames(x)[[1]])>=min]
            
            if (!is.na(plusgroup)){
              x=setPlusGroup(x,plusgroup) 
            }else{
              x=x[as.numeric(dimnames(x)[[1]])<=max]}
        
        range(x)[c("minyear","maxyear","min","max","plusgroup")]=c(start,end,min,max,plusgroup)
            
        x})

if(FALSE){
data(ple4)
range(ple4)[c("minyear","maxyear","min","max","plusgroup","Minfbar")]=c(1980,2010,5,7,7,5)
plot(window(ple4))
}