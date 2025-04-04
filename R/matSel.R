findAge<-function(data, age=seq(length(data))-1, target=0.5) {
  # Check for edge cases
  if(all(data < target) || all(data > target))
    return(NA)  # Target value outside range of data
  
  # Ensure the vectors are sorted by age
  sorted<-order(age)
  age   <-age[sorted]
  data  <-data[sorted]
  
  # Use approx to interpolate
  interp<-approx(x=data, y=age, xout=target)
  
  return(interp$y)}

# Helper function to calculate area of overlap
calcOverlap<-function(curve1, curve2) {
  # Normalize curves
  norm1<-curve1 / max(curve1)
  norm2<-curve2 / max(curve2)
  
  # Calculate overlap
  minSum<-sum(pmin(norm1, norm2))
  maxSum<-sum(pmax(norm1, norm2))
  
  return(minSum/maxSum)}

#' Calculate metrics comparing maturity and selectivity patterns
#' 
#' @param x An FLStock object containing catch.sel and mat slots
#' @return A data frame with age-based metrics comparing selectivity and maturity
matSel<-function(x) {
  # Extract and format maturity and selectivity data
  metrics<-model.frame(FLQuants(x,sel=function(x) catch.sel(x)/max(catch.sel(x)), 
                                  mat=mat), drop = TRUE)
  
  # Calculate cumulative distributions and differences
  suppressWarnings(metrics<-metrics %>%
    mutate(cumSel   =cumsum(sel)/max(cumsum(sel)),
           cumMat   =cumsum(mat)/max(cumsum(mat)),
           dif      =cumSel - cumMat,
           mat50    =findAge(mat, age),
           sel50    =findAge(sel, age),
           maxDifAge=age[which.max(dif)],
           maxDifVal=max(dif)))
  
  return(metrics)}

selMetrics<-function(x) {
  dat=ldply(x, matSel)
  
  summary<-dat %>%
    group_by(.id) %>%
    summarize(
      ageMaxDif=age[which.max(dif)][1],
      mat50    =first(mat50),
      sel50    =first(sel50),
      selMatLag=sel50-mat50,
      vuln     =sum(sel*mat)/sum(mat),
      overlap  =calcOverlap(sel, mat))
  
  return(summary)}

plotMatSel<-function(x) {
  # Create scatterplot of sel50 vs mat50
  ggplot(x, aes(x = mat50, y = sel50)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Age at 50% Maturity", y = "Age at 50% Selectivity",
         title = "Comparison of Maturity and Selectivity Patterns") +
    theme_minimal()}
