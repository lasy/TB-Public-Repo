
package_list = c('knitr',
                 'tidyverse','readr',
                 'data.table',
                 'MASS',
                 'hexbin','ggthemes','ggplot2','mapdata','gridExtra','quantreg',
                 # 'plotly',
                 'scales','reshape',
                 'HMM',
                 'chron','lubridate',
                 'plyr','dplyr',
                 'foreach',
                 'parallel','doParallel',
                 'tictoc',
                 'styler',
                 'bookdown',
                 'klaR', 'cluster','factoextra',
                 'Rcpp',
                 'e1071', 'parallelSVM', 'randomForest','SwarmSVM',
                 'ggalluvial'
)

pckgs = installed.packages()

for(package in package_list){
  cat(package,"\n")
  pckgs = installed.packages()
  need.to.install = (!(package %in% pckgs[,1]))
  if(need.to.install){cat('installing package ',package,'\n');install.packages(package,dependencies = TRUE)}
  library(package = package, character.only = TRUE)
}

rm(package_list, pckgs,need.to.install,package)


