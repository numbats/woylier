# Generate sine wave data
library(tidyverse)
set.seed(5543)
sinData <- function(n, p){
  vName <- paste0("V",n)
  vNameM1 <- paste0("V",n-1)
  expr <- paste0(vName,"=sin(",vNameM1,")") # need string expression if I want to use tibble here
  dRet <- as_tibble(matrix(rnorm((n-1)*p), ncol=(n-1))) #generate normal distributed n-1 dim data
  dRet <- mutate_(dRet, expr) #string evaluation calculates var(n) as tan(var(n-1))
  colnames(dRet)[n] <- vName #correct name of new variable
  dRet[vName] <- jitter(dRet[[vName]]) #adding noise
  return(dRet)
}

sine_curve <- as.data.frame(sinData(6, 500))
sine_curve <- sine_curve %>%
  mutate(across(V1:V6, function(x) ((x-min(x))/(max(x)-min(x))*2-1)))

save(sine_curve, file="data/sine_curve.rda")
