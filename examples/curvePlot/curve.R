
rplist1=SS_output("P:/rfmo/ices/wkbseabass/ss3/north/0.7/0.2")
dt1=curveSS(rplist1)

# Plot yield curve with geom_rect
dat=dt1
ggplot(dat$rfs)+
  geom_rect(aes(xmin=0,    xmax=bmsy, ymin=msy, ymax=Inf),fill="red",    alpha=0.5)+
  geom_rect(aes(xmin=bmsy, xmax=Inf,  ymin=0,   ymax=msy),fill="green",  alpha=0.5)+
  geom_rect(aes(xmin=0,    xmax=bmsy, ymin=0,   ymax=msy),fill="yellow", alpha=0.5)+
  geom_rect(aes(xmin=bmsy, xmax=Inf,  ymin=msy, ymax=Inf),fill="yellow", alpha=0.5)+
  geom_polygon(data=dat$trgl, aes(x=x, y=y),              fill="orange", alpha=0.5)+
  geom_line( data=dat$eql, aes(ssb, yield), color = "blue")+
  geom_path( data=dat$ts,  aes(ssb, yield), color = "black") +
  geom_point(data=dat$ts,  aes(ssb, yield)) +
  geom_point(data=subset(dat$ts,year==max(year)), aes(ssb, yield), color = "red", size=2.5) +
  geom_point(data=subset(dat$ts,year==min(year)), aes(ssb, yield), color = "blue",size=2.5) +
  labs(title="Yield vs SSB", x="Total Biomass (t)", y="Production (t)") +
  geom_abline(data=dat$rfs, aes(slope=msy/bmsy, intercept=0), col="grey10", linetype=3)+
  theme_minimal() +
  theme(legend.position="none")+
  coord_cartesian(ylim=c(0,max(dat$trgl$y)),expand=FALSE)

pfs=setNames(mlply(with(scen,file.path("P:/rfmo/ices/wkbseabass/ss3/north",s,M)), 
                   function(x) FLCandy:::tryIt(curveSS(SS_output(x)))), seq(32))
pfs=Map(function(x) cbind(x, scen[x$Scenario,]), unnest(pfs))

ggplot(pfs$rfs)+
  facet_grid(s~M, labeller = labeller(
    M=function(x) paste("M =", x),
    s=function(x) paste("s =", x)),
    scale="free",space="free")+
  # Base layers
  geom_polygon(data=dat$trgl,aes(x, y),                 fill="orange",  alpha=0.5)+
  geom_rect(aes(xmin=0,    xmax=bmsy,ymin=msy,ymax=Inf),fill="#D55E00", alpha=0.3)+
  geom_rect(aes(xmin=bmsy, xmax=Inf, ymin=0,  ymax=msy),fill="#009E73", alpha=0.3)+
  geom_rect(aes(xmin=0,    xmax=bmsy,ymin=0,  ymax=msy),fill="#F0E442", alpha=0.3)+
  geom_rect(aes(xmin=bmsy, xmax=Inf, ymin=msy,ymax=Inf),fill="#F0E442", alpha=0.3)+
  coord_cartesian(ylim =c(0,8e3), expand = FALSE)+
  # Lines 
  geom_line(data = dat$eql, aes(ssb, yield), color="blue")+
  geom_abline(data=dat$rfs, aes(slope=msy/bmsy, intercept=0), col="grey10", linetype=3) +
  # Time series
  geom_point(data = subset(dat$ts, year %in% c(min(year), max(year))),
             aes(ssb, yield, color = factor(year)))+
  geom_point(data = subset(dat$ts, !(year %in% c(min(year), max(year)))),
             aes(ssb, yield), color="grey10",fill="grey90", shape=21, size=1, stroke=0.2)+
  geom_path(data=dat$ts, aes(ssb, yield),
            arrow = arrow(length = unit(0.3, "cm"), type="closed", angle=30,ends = "last"),
            size = 0.25,color="grey10")+
  # Reference points
  geom_point(aes(x=bmsy, y=msy), shape=21, fill="white", size=3) +
  geom_text( aes(x=bmsy, y=msy,  label="MSY"), hjust=-0.2, vjust=-0.2) +
  # Scales
  #scale_x_continuous(labels=scales::comma,breaks=seq(0, 100000,by=20000),
  #                   expand=expansion(mult=c(0, 0.05))) +
  scale_y_continuous(labels=scales::comma) +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Start", "End"),
    name = "Time Period") +
  # Labels
  labs(title="Yield vs SSB",
       x    =expression("Total Biomass (t)"),
       y    =expression("Production (t)"))+
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        axis.text.x=element_text(angle =45,
                                 hjust =1,
                                 size  =10,
                                 margin=margin(t=10)),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.line = element_line(color = "black"))

tmp=mlply(with(scen,file.path("P:/rfmo/ices/wkbseabass/ss3/north",s,M)), 
          function(x) smrySS(SS_output(x)))
names(tmp)=seq(32)
dt2=Map(function(x) cbind(x, scen[x$Scenario,]), unnest(tmp))


kobePhase(dt2$kb,xlim=c(0,5),ylim=c(0,1.25))+
  facet_grid(s~M, labeller = labeller(
    M=function(x) paste("M =", x),
    s=function(x) paste("s =", x)))+
  geom_path(aes(stock, harvest), color="blue") +
  #geom_path(data=dat$kb, aes(stock, harvest), 
  #          arrow = arrow(length = unit(0.3, "cm"), type="closed", angle=30, ends = "last"),
  #          size = 0.25, color="grey10")+
  geom_point(data = subset(dt2$kb, !(year %in% c(min(year), max(year)))),
             aes(stock, harvest), color="grey10", fill="grey90", 
             shape=21, size=1, stroke=0.2)+
  geom_point(data = subset(dt2$kb, year %in% c(min(year), max(year))),
             aes(stock, harvest, color = factor(year)))+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Start", "End"),
                     name = "Time Period")

  