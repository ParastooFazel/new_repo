### Perform median imputation
install.packages("ape")
library(Hmisc)

MedianImpute <- function(data, time){ # inputs: data.frame and name of column with time
  splt <- split(data, data[, t])
  do.call(rbind, lapply(splt, function(x) apply(x,2,impute)))
} 

## example
df <- data.frame(t=rep(c(0,30,60,90,120),each=5), y1=rnorm(25), y2=rnorm(25)) # dataset
df$y1[sample(nrow(df),5)] <- NA; df$y2[sample(nrow(df),5)] <- NA # introduce random missings  
MedianImpute(df, 't') # impute!
