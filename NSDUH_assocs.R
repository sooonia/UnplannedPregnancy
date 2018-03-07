library(caret)
options(java.parameters = "-Xmx8000m")
library(RWeka)
load('NSDUH-2016-DS0001-bndl-data-r/NSDUH-2016-DS0001-data/NSDUH-2016-DS0001-data-r.RData')
PUF2016_101617 <- droplevels(PUF2016_101617[PUF2016_101617$pregnant == 1,])
#Subset only to women
#PUF2016_101617 <- droplevels(PUF2016_101617[PUF2016_101617$irsex == 2,])
#studs <- droplevels(PUF2016_101617[-which(PUF2016_101617$yestscig == 99),])



drugREC <- names(PUF2016_101617[, grepl("rec", names(PUF2016_101617))])
numeric <- c('BMI2')
#school <- c('yeschact','snrlgsvc','snrlgimp', 'avggrade', 'yestscig', 'yestsmj',
#            'yestsalc', 'yeplmtsn','yepgdjob','yeyargup', 'yeothact')
predVars <- c('NOMARR2', 'sexatract', 'speakengl', 'irmarit', 'catage',
              'NEWRACE2', 'eduhighcat', 'HEALTH2', 'stdanyyr', 'irwrkstat', 'IRKI17_2',
              'caidchip', 'prvhltin','income')
PUF2016_101617 <- PUF2016_101617[,predVars]
#studs <- studs[,c(predVars,school)]
for(factor in predVars){
  PUF2016_101617[,factor] <- as.factor(PUF2016_101617[, factor])
}

#test <- PUF2016_101617[sample(nrow(PUF2016_101617), size = 10000),]
Apriori(PUF2016_101617, control = Weka_control(N = 50))