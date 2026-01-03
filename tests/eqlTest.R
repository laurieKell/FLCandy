library(remotes)

remotes::install_github("laurieKell/FLCandy",  force=TRUE)

if (FALSE){  
  install.packages("C:/active/flr/FLCandy",repos=NULL,type="source")
}

myDir="C:/active/tmp"

load("C:/active/flrpapers/brebuild/data/icesdata.RData")

library(FLCore)
library(FLBRP)
library(ggplotFL)
library(FLCandy)

data(ple4)

FLCandy::eql(ple4,model="bevholtSV",prior_s=0.7,cv_s=0.1)
