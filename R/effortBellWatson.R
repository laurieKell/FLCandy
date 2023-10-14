library(ggplot2)
library(plyr)

year=seq(1950,2012)

asia=data.frame(year=c(1950,1970,1995,2012),
                  E   =c(10,    10, 200, 250)/250)
asia=merge(cbind(asia,.id=seq(4)+1),cbind(asia,.id=seq(4)),by=".id")
asia=ddply(asia, .(.id), with, data.frame(F=seq(E.x,E.y,length.out=year.y-year.x+1),year=seq(year.x,t=year.y)))

europe=data.frame(year=c(1950,1960,1980,1990,2012),
                E     =c(  10,  10,  15,  15, 10)/15)
europe=merge(cbind(europe,.id=seq(5)+1),cbind(europe,.id=seq(5)),by=".id")
europe=ddply(europe, .(.id), with, data.frame(F=seq(E.x,E.y,length.out=year.y-year.x+1),year=seq(year.x,t=year.y)))

africa=data.frame(year=c(1950,1990,2012),
                E     =c(5,    5,    40)/40)
africa=merge(cbind(africa,.id=seq(3)+1),cbind(africa,.id=seq(3)),by=".id")
africa=ddply(africa, .(.id), with, data.frame(F=seq(E.x,E.y,length.out=year.y-year.x+1),year=seq(year.x,t=year.y)))

samerica=data.frame(year=c(1950,1980, 1990,  2012),
                E       =c(   1,   1,  1.5,   7.5)/7.5)
samerica=merge(cbind(samerica,.id=seq(4)+1),cbind(samerica,.id=seq(4)),by=".id")
samerica=ddply(samerica, .(.id), with, data.frame(F=seq(E.x,E.y,length.out=year.y-year.x+1),year=seq(year.x,t=year.y)))

namerica=data.frame(year=c(1950,1970,  1975,1980,2012),
                    E   =c(   5,   5,   2.5, 2.5,  10)/10)
namerica=merge(cbind(namerica,.id=seq(5)+1),cbind(namerica,.id=seq(5)),by=".id")
namerica=ddply(namerica, .(.id), with, data.frame(F=seq(E.x,E.y,length.out=year.y-year.x+1),year=seq(year.x,t=year.y)))

oceania=data.frame(year=c(1950,1970,2000,2012),
                E       =c( 2.5, 1, 1.25, 3)/3)
oceania=merge(cbind(oceania,.id=seq(4)+1),cbind(oceania,.id=seq(4)),by=".id")
oceania=ddply(oceania, .(.id), with, data.frame(F=seq(E.x,E.y,length.out=year.y-year.x+1),year=seq(year.x,t=year.y)))

F=rbind(cbind("Region"="Asia",         asia),
        cbind("Region"="Oceania",      oceania),
        cbind("Region"="South America",samerica),
       #cbind("Region"="North America",namerica),
        #cbind("Region"="Europe",       europe),
        cbind("Region"="Africa",       africa)
        )[,-2]

ref =ddply(subset(F,year>=1980), .(Region), with,  data.frame(year=max(year[(F-0.5)^2==min((F-0.5)^2)])))
ref2=ddply(subset(F,year>=1980), .(Region), with,  data.frame(year=max(year[(F-0.6)^2==min((F-0.6)^2)])))


# The exploitation rate (E) is used as indicator expressing the status of stocks, 
# a stock is considered as under-exploited if E<0.5, 
# over-exploited if E>0.6 and
# the target E is in the range 0.5-0.6.

ggplot(subset(F,Region!="Europe"))+
  geom_polygon(aes(x,y,fill=Zone),data=data.frame(x   =rep(c(1950,2012,2012,1950,1950),3),
                                             y   =c(0,0,1,1,0, 1,1,1.2,1.2,1, 1.2,1.2,2,2,1.2),
                                            Zone=rep(c("UnderFished","Sustainable","Overfished"),each=5)))+
  geom_line( aes(year,F*2,col=Region),size=2)+
  geom_point(aes(year,F*2),data=merge(F,ref))+
  scale_fill_manual(values=c("red","green","blue"))+
  xlab("Year")+ylab(expression(F:F[MSY]))
  

sbh<-function(S,a,b,c) a/(1+(b/S)^c)

ggplot(data.frame(ssb=seq(1,10,length.out=100),rec=sbh(seq(1,10,length.out=100),a=1,b=25,c=0.5)))+
  geom_line(aes(ssb,rec))







