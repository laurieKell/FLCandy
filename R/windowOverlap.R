library(plyr)
windowOverlap = function(dat, yearCol = "year", step = 10, overlap=5) {
  
  years = dat[[yearCol]]
  yMin  = min(years, na.rm = TRUE)
  yMax  = max(years, na.rm = TRUE)
  
  centres = seq(
    from = ceiling(yMin / step) * step,
    to   = floor(yMax / step) * step,
    by   = step
  )
  
  outList = lapply(centres, function(y0) {
    from = y0 - overlap
    to   = y0 + 9+overlap
    
    sel = years >= from & years <= to
    w   = dat[sel, ]
    w$centre = y0
    w$from   = from
    w$to     = to
    w
  })
  
  names(outList) = as.character(centres)
  
  out = ldply(outList, .id = ".id")
  out}
