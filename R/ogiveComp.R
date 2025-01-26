# Define the generic function
setGeneric("compareOgives", function(ogive1, ogive2, ...) {
  standardGeneric("compareOgives")})

# Define method for FLQuant objects
setMethod("compareOgives", signature(ogive1="FLQuant", ogive2="FLQuant"),
      function(ogive1, ogive2) {
        # Ensure dimensions match
        if (!all(dim(ogive1) == dim(ogive2))) 
          stop("Dimensions of ogive1 and ogive2 must match.")
            
        # Calculate cumulative proportions
        cumOgive1=apply(ogive1, 2:6, cumsum) %/% apply(ogive1, 2:6, sum)
        cumOgive2=apply(ogive2, 2:6, cumsum) %/% apply(ogive2, 2:6, sum)
  
        res=ddply(model.frame(FLQuants("o1"=cumOgive1,"o2"=cumOgive2),drop=TRUE),.(year), with, {
          data.frame("ks.p" =ks.test(o1,o2)$p.value,
                     "hDist"=sqrt(0.5*sum((sqrt(o1)-sqrt(o2))^2)),
                     "area" =sum(abs(o2-o1)[-1] +abs(o2-o1)[-length(o2)])/2,
                     "diff" =(o2-o1)[max(abs(o2-o1))==abs(o2-o1)])})
        
        res=FLQuants(ks.p =as.FLQuant(transmute(res,year=year,data=ks.p )),
                     hDist=as.FLQuant(transmute(res,year=year,data=hDist)),
                     area =as.FLQuant(transmute(res,year=year,data=area )),
                     diff =as.FLQuant(transmute(res,year=year,data=diff )))
        
        return(res)})



oCtc=apply(catch.sel(object),2:6,cumsum)%/%apply(catch.sel(object),2:6,cumsum)[dim(m(object))[1]]
oMat=apply(mat(object),2:6,cumsum)%/%apply(mat(object),2:6,cumsum)[dim(m(object))[1]]

ggplot(oCtc)+
  geom_line(aes(age,data,col=year,group=year))+
  geom_line(aes(age,data),data=as.data.frame(oMat),col="red")

ggplot(oCtc%-%oMat)+
  geom_line(aes(age,data,col=year,group=year))

ggplot(oCtc)+
  geom_line(aes(year,data,group=ac(age),col=ac(age)),
            data=as.data.frame(oMat))+
  geom_line(aes(year,data,group=ac(year-age)))+
  geom_point(aes(year,data,col=ac(age)))+
  xlab("Year")+ylab("Cumulative Ogive")


ggplot(hd)+geom_line(aes(year,hDist))


# Plot the comparison
plot_data <- rbind(
  data.frame(age = ages, proportion = ogive1_df$proportion, type = "Catch Selectivity"),
  data.frame(age = ages, proportion = ogive2_df$proportion, type = "Maturity Ogive")
)

ggplot(plot_data, aes(x = age, y = proportion, color = type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = pmin(cumOgive1[, 1], cumOgive2[, 1]),
                  ymax = pmax(cumOgive1[, 1], cumOgive2[, 1])),
              fill = "gray", alpha = 0.3) +
  labs(
    title = "Comparison of Catch Selectivity and Maturity Ogives",
    x = "Age",
    y = "Cumulative Proportion",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Catch Selectivity" = "blue", "Maturity Ogive" = "red"))
