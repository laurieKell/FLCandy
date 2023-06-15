
dat=c(a= 0.28, a=(0.48- 0.07)/(2 * 1.96),
      b=-1.30, b=(- 1.19 +1.42)/(2 * 1.96),
      c= 1.08, c=(1.24 - 0.92)/(2 * 1.96))

par=FLPar(dat[seq(1,6,2)])
sd =FLPar(dat[seq(2,6,2)])

par=rbind(rnorm(500,par["a"],sd["a"]),
          rnorm(500,par["b"],sd["b"]),
          rnorm(500,par["c"],sd["c"]))

lh  =FLPar(c(linf=40,k=0.5,l50=18))
lhSd=lh/5

lh=lhPar(rbind(rnorm(100,lh["linf"],lhSd["linf"]),
               rnorm(100,lh["k"],   lhSd["k"]),
               rnorm(100,lh["l50"], lhSd["l50"])))
eq=lhEql(lh)

m50=exp(par["a"]%+%par["b"]%*%log(lh["l50"]%/%lh["linf"])%+%par["c"]%*%log(lh["k"]))   

dimnames(m50)[[1]]="M"
plot(m50)

priors=popdyn(lh)



  

