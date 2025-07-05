load(file.path(path, label, "ss.RData"))

  # Extract hat & standard error
  par=subset(ss$par,Label==label)

  # Get LLs 
  ll      =ss$likelihoods_used[1, "values"]
  deltaNLL=ll-min(ll,na.rm=TRUE)

  
  # Create quadratic curve based on Hessian SEs 
  vals=seq(min(profile$hat,na.rm=TRUE), max(profile$hat,na.rm=TRUE),length.out=101)
  quadCurve=(vals-hat)^2/(2*se^2)

# Plot the comparison
ggplot() +
  geom_line(data=data.frame(F=Fseq, deltaNLL=quadCurve),
            aes(x=F, y=deltaNLL), color="red", linewidth=1) +
  geom_point(data=profile,
             aes(x=F, y=deltaNLL), color="blue", size=2) +
  geom_vline(xintercept=Fhat, linetype="dashed") +
  labs(title="Comparison of Uncertainty Methods for B/BMSY",
       y="Change in negative log-likelihood", 
       x="F/FMSY")+
  theme_minimal()
