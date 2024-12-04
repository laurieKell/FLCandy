# Required packages
require(ss3diags)
require(r4ss)

# Function to run SS3 diagnostics
ssDiagnostics<-function(path, name, plot = TRUE) {
  # Create diagnostics plot directory if plotting enabled
  if(plot) dir.create("Plotdiags", showWarnings = FALSE)
  
  # Load model
  ss3rep=SS_output(dir = path, covar = TRUE)
  save(ss3rep, file = paste0(name, ".Rdata"))
  
  if(plot) {
    # Basic R4SS plots
    SS_plots(ss3rep, dir = getwd())
    
    # Data setup plots
    sspar()
    SSplotData(ss3rep, subplots = 2)
    dev.print(jpeg, paste0("Plotdiags/DataSetup_", name, ".jpg"), 
              width = 8, height = 6, res = 300, units = "in")
    
    # Residual diagnostics
    sspar(mfrow = c(2,4), plot.cex = 0.8)
    SSplotRunstest(ss3rep, subplots = "cpue", add = TRUE, legendcex = 0.6, mixing = "two.sided")
    SSplotRunstest(ss3rep, subplots = "age",  add = TRUE, legendcex = 0.6, mixing = "less")
    dev.print(jpeg, paste0("Plotdiags/RunsTestResiduals_", name, ".jpg"), 
              width = 8, height = 7, res = 300, units = "in")
    
    # Joint residuals
    sspar(mfrow = c(1,2), plot.cex = 0.8)
    SSplotJABBAres(ss3rep, subplots = "cpue", add = TRUE, col = sscol(3)[c(1,3,2)])
    SSplotJABBAres(ss3rep, subplots = "age", add = TRUE, col = sscol(3)[c(1,3,2)])
    dev.print(jpeg, paste0("Plotdiags/JointResiduals_", name, ".jpg"), 
              width = 8, height = 3.5, res = 300, units = "in")
    
    # MVLN uncertainty
    sspar(mfrow = c(1,1), plot.cex = 0.9)
    mvn=SSdeltaMVLN(ss3rep, plot = TRUE, Fref = c("Btgt"), catch.type = c("Exp"))
    kbproj=data.frame(mvn$kb)
    SSplotKobe(kbproj, fill = TRUE, joint = FALSE, posterior = "kernel",
               ylab = expression(F/F[trg]), xlab = expression(SSB/SSB[trg]))
    dev.print(jpeg, paste0("Plotdiags/Kobe_", name, ".jpg"), 
              width = 6.5, height = 6.5, res = 300, units = "in")
  } else {
    mvn=SSdeltaMVLN(ss3rep, plot = FALSE, Fref = c("Btgt"), catch.type = c("Exp"))
  }
  
  return(list(model = ss3rep, mvln = mvn))}

# Function to run retrospective analysis
ssRetrospective<-function(retros, name, plot = TRUE) {
  retroSummary=r4ss::SSsummarize(retro_models)
  hccomps=SSretroComps(retro_models)
  
  if(plot) {
    # Retrospective plots
    sspar(mfrow = c(2,2), plot.cex = 0.9)
    SSplotRetro(retroSummary, forecastrho = TRUE, add = TRUE, subplots = "SSB")
    SSplotRetro(retroSummary, forecastrho = TRUE, add = TRUE, legend = FALSE)
    SSplotRetro(retroSummary, subplots = "F", add = TRUE, legendloc = "left", legendcex = 0.8)
    SSplotRetro(retroSummary, subplots = "F", forecastrho = TRUE, add = TRUE, legend = FALSE)
    dev.print(jpeg, paste0("Plotdiags/RetroForecast_", name, ".jpg"), 
              width = 8, height = 9, res = 300, units = "in")
    
    # Hindcast cross-validation
    sspar(mfrow = c(1,2), plot.cex = 0.9)
    SSplotHCxval(retroSummary, add = TRUE, legendcex = 0.6, Season = 1)
    dev.print(jpeg, paste0("Plotdiags/HCxvalIndex_", name, ".jpg"), 
              width = 8, height = 5, res = 300, units = "in")
  }
  
  return(list(retroSummary = retroSummary, hccomps = hccomps))}

# For basic diagnostics
#model=ssDiagnostics(path="P:/rfmo/ices/wkbseabass/ss3/north/base",name="test")

# For retrospective analysis (if you have retro models)
#retro_results=ssRetrospective(retros=, name="test")