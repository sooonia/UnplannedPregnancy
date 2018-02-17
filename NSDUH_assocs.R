library(caret)
library(RWeka)
load('NSDUH-2016-DS0001-bndl-data-r/NSDUH-2016-DS0001-data/NSDUH-2016-DS0001-data-r.RData')

pre <- preProcess(PUF2016_101617, method=c('zv', 'nzv'), na.remove=TRUE)
td <- predict(pre, PUF2016_101617)
remove(PUF2016_101617)

td[colnames(td)] <- lapply(td[colnames(td)], factor)
preg <- td[td$pregnant == 1,]
Apriori(preg, control = (N=30))