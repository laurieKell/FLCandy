library(ggplot2)

hcrPlot2<-function(btarget=1.0,bmax=1.5,blim=0.1,bthresh=0.8,btrigger=0.9,
                   ftarget=0.8,fmax=1.5,fmsy=1.0,
                   textSize=4){
  
  bref=c(target=btarget,max=bmax,lim=blim,thresh=bthresh,trigger=btrigger)
  fref=c(target=ftarget,max=fmax,msy=fmsy)
  
  regions=data.frame(
    xmin=c( 0.00, bref["lim"], bref["thresh"], bref["thresh"],  bref["thresh"]),
    xmax=c( bref["lim"], bref["thresh"], bref["max"], bref["max"], bref["lim"]),
    ymin=c(-0.03,-0.03,-0.03, fref["msy"], fref["msy"]),
    ymax=c( fref["max"], fref["max"], fref["msy"], fref["max"], fref["max"]),
    fill=c("Collapsed","Rebuilding", "Sustainable", "Overfishing","Overfished"))
  
  region_colors=c(
    "Collapsed"  ="red",
    "Overfished" ="red",
    "Overfishing"="orange",
    "Rebuilding" ="yellow",
    "Sustainable"="green")
  
  p=ggplot()+
    geom_rect(data=regions,
              aes(xmin=xmin, xmax=xmax,
                  ymin=ymin, ymax=ymax,
                  fill=fill), alpha=1)+
    scale_fill_manual(values=region_colors)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    geom_vline(xintercept=c(bref["lim"]), linetype="dashed")+
    geom_hline(yintercept=c(1),   linetype="solid")+
    geom_line(aes(x=rep(bref["target"],2),y=c(-0.03,fref["target"])))+
    geom_line(aes(x=c(0,bref["lim"],bref["thresh"],bref["max"]), y=c(0,0,rep(fref["target"],2))),col="steelblue",linewidth=1)+
    geom_line(aes(x=c(0,0.4,bref["thresh"]), y=c(0,0.0,fref["target"])),col="steelblue",linewidth=1)+
    geom_line(aes(x=c(0,0.6,bref["thresh"]), y=c(0,0.0,fref["target"])),col="steelblue",linewidth=1)+
    labs(x=expression(B/B[target]),
         y=expression(F/F[target]))+
    theme(legend.position="none")+
    annotate("text", label="Collapsed",        x=bref["lim"]*0.5,   y=fref["msy"]*0.5,vjust=0.5,hjust=1,  angle=90, size=textSize)+
    annotate("text", label="Overfished",       x=bref["thresh"]*0.5,y=(fref["msy"]+fref["max"])*0.5,vjust=0,  hjust=0,  angle=0,  size=textSize)+
    annotate("text", label="Overfishing",      x=btarget,           y=(fref["msy"]+fref["max"])*0.5,vjust=0,  hjust=0,            size=textSize)+
    annotate("text", label="Rebuilding",       x=bref["thresh"]*0.5,y=fref["msy"]*0.5, vjust=0,  hjust=0,  angle=0,  size=textSize)+
    annotate("text", label=expression(B[lim]), x=bref["lim"],    y=fref["target"], vjust=0,  hjust=1,            size=textSize)+
    annotate("text", label=expression(B[thr]), x=bref["thresh"], y=fref["target"], vjust=0,  hjust=1,            size=textSize)+
    annotate("text", label=expression(B[tgt]), x=bref["target"], y=fref["target"], vjust=0,  hjust=1,            size=textSize)+
    annotate("text", label="Sustainable",      x=bref["target"], y=fref["msy"]*0.5, vjust=0,  hjust=0,  angle=0,  size=textSize)+ 
    annotate("text", label=expression(F[tgt]), x=bref["max"],    y=fref["target"], vjust=0,  hjust=1,            size=textSize)
  
    p}

################################################################################
hcrPlot<-function(btarget=1.0,bmax=1.5,blim=0.1,bthresh=0.8,btrigger=0.9,
                  ftarget=1.0,fmax=1.5,fadv=ftarget,
                  textSize=4){
  
  bref=c(target=btarget,max=bmax,lim=blim,thresh=bthresh,trigger=btrigger)
  fref=c(target=ftarget,max=fmax,adv=fadv)
  
  btarget=bref["target"]
  if (is.na(btarget)) btarget=1
  
  regions=data.frame(
    xmin=c(0.00, bref["lim"], bref["thresh"]),
    xmax=c(      bref["lim"], bref["thresh"], bref["max"]),
    ymin=c(0.00, 0.00, 0.00),
    ymax=rep(fref["max"], 3),
    fill=c("Critical", "Overfished", "Overfishing"))
  
  region1=data.frame(
    x=c(0, bref["lim"],bref["lim"],0),
    y=c(0, fref["target"]*bref["lim"]/bref["trigger"], 0,0))
  region2=data.frame(
    x=c(0, bref["thresh"],bref["thresh"],0),
    y=c(0, fref["target"]/bref["trigger"]*bref["thresh"], 0,0))
  region3=data.frame(
    x=c(bref["thresh"],bref["thresh"],bref["trigger"],bref["max"],bref["max"],bref["thresh"]),
    y=c(0,             fref["target"]*bref["thresh"]/bref["trigger"],fref["target"], fref["target"], 0,0))
    
  region_colors=c(
    "Critical"   ="brown",
    "Overfished" ="red",
    "Overfishing"="orange",
    "Rebuilding" ="yellow",
    "Sustainable"="green")
  
  # Create the plot
  p=ggplot()+
    geom_rect(data=regions,
              aes(xmin=xmin, xmax=xmax,
                  ymin=ymin, ymax=ymax,
                  fill=fill), alpha=1)+
    geom_polygon(data=region2,
                  aes(x,y),fill="yellow")+
    geom_polygon(data=region3,
                  aes(x,y),fill="green")+
    geom_polygon(data=region1,
                 aes(x,y),fill="brown3")+
    scale_fill_manual(values=region_colors)+
    scale_x_continuous(expand=c(0, 0), limits=c(0, bref["max"]))+
    scale_y_continuous(expand=c(0, 0), limits=c(0, fref["max"]))+
    geom_line(aes(x=c(0,bref["trigger"],bref["trigger"]),
                  y=c(  fref["target"], fref["target"],0)), color="black",linetype="dashed")+
    geom_line(aes(x=c(0,bref["trigger"],bref["max"]),
                   y=c(0,fref["target"], fref["target"])), color="black")+
    geom_line(aes(x=rep(bref["lim"],2),
                  y=c(0,bref["lim"]*fref["target"]/bref["trigger"])), color="black")+
    geom_line(aes(x=rep(bref["thresh"],2),
                  y=c(0,bref["thresh"]*fref["target"]/bref["trigger"])), color="black")
  
  p=p+
    annotate("text", label=expression(B[lim]),      x=bref["lim"],   y=fref["target"], hjust=1, vjust=0,size=textSize)+
    annotate("text", label=expression(B[threshold]),x=bref["thresh"],y=fref["target"]/bref["trigger"]*bref["thresh"], 
                                                                                       hjust=1, vjust=0,size=textSize)+
    annotate("text", label=expression(B[trigger]), x=bref["trigger"],y=fref["target"], hjust=1, vjust=0,size=textSize)+
    annotate("text", label=expression(F[target]),  x=bref["max"],    y=fref["target"], hjust=1, vjust=0,size=textSize)+
    annotate("text", label="Critical",             x=0.05, y=bref["thresh"], hjust=0.5, vjust=0.5,angle=90,size=textSize)+
    annotate("text", label="Rebuilding",           x=0.50, y=0.25, hjust=0.5, vjust=1,size=textSize)+
    annotate("text", label="Sustainable",          x=(btarget+bref["max"])/2,  y=0.25, hjust=0.5, vjust=1,size=textSize)+
    annotate("text", label="Overfished",           x=0.50, y=1.35, hjust=0.5, vjust=1,size=textSize)+
    annotate("text", label="Overfishing",          x=(btarget+bref["max"])/2,  y=1.35, hjust=0.5, vjust=1,size=textSize)+
    labs(x=expression(B/B[target]),
         y=expression(F/F[target]))+
    theme_minimal()+theme(
      legend.position="none",
      axis.text.x=element_text( size=10),
      axis.text.y=element_text( size=10),
      axis.title.x=element_text(size=12),
      axis.title.y=element_text(size=12))
  
    if (!is.na(bref["target"])){ 
       p=p+geom_line(aes(x=rep(btarget,2),
                  y=c(0,fref["target"])), color="blue")+
       annotate("text", label=expression(B[target]),  x=btarget, y=fref["target"], hjust=1, vjust=0,size=textSize)}
    
     suppressWarnings(p)}
