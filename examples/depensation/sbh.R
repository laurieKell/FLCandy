
sbh<-function(S,a,b,c) a/(1+(b/S)^c)

ggplot(data.frame(ssb=seq(1,10,length.out=100),rec=sbh(seq(1,10,length.out=100),a=1,b=25,c=0.5)))+
  geom_line(aes(ssb,rec))