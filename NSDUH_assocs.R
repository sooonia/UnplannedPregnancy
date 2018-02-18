library(caret)
options(java.parameters = "-Xmx8000m")
library(RWeka)
load('NSDUH-2016-DS0001-bndl-data-r/NSDUH-2016-DS0001-data/NSDUH-2016-DS0001-data-r.RData')
PUF2016_101617 <- droplevels(PUF2016_101617[PUF2016_101617$pregnant == 1,])

pre <- preProcess(PUF2016_101617, method=c('zv', 'nzv'), na.remove=TRUE)
PUF2016_101617 <- predict(pre, PUF2016_101617)
#remove(PUF2016_101617)

PUF2016_101617[colnames(PUF2016_101617)] <- lapply(PUF2016_101617[colnames(PUF2016_101617)], factor)
PUF2016_101617 <- subset(PUF2016_101617, select = -filedate )

Apriori(PUF2016_101617[sample(nrow(PUF2016_101617), 500), ], control = c(N=10))