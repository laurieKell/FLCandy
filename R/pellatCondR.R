# Calculates r for Pella-Tomlinson (with 1/p) given virgin, fmsy, bmsy, and B
calcR<-function(virgin, fmsy, bmsy, B) {
  # Estimate p from bmsy and virgin
  p=log(1/(bmsy/virgin))/log(bmsy/virgin)
  # Calculate r for the alternative formulation (with /p in denominator)
  r=fmsy/((1-(bmsy/virgin)^p)/p)
  # Surplus production at B
  SP=r*B*(1-(B/virgin)^p)/p
  
  return(SP/B)}

calcR(virgin=10000, fmsy=0.2, bmsy=4000, B=5000)

