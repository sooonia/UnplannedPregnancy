pram10<-read.csv("/users/amberlehman/Documents/GitHub/UnplannedPregnancy/PRAMdata/CDC_PRAMStat_Data_for_2010.csv")
load('/users/amberlehman/Documents/GitHub/UnplannedPregnancy/PRAMdata.rda')
levels(pram10$LocationAbbr)
levels(rd$LocationAbbr)
pram09<-read.csv("/users/amberlehman/Documents/GitHub/UnplannedPregnancy/PRAMdata/CDC_PRAMStat_Data_for_2009.csv")
rd<-rbind(rd,pram10)
rd<-rbind(rd,pram09)
unintended <- rd[rd$Break_Out == 'Unintended',]
intended <- rd[rd$Break_Out == 'Intended',]


unintended.bp <- droplevels(unintended[unintended$QuestionId == 'QUO3',])
intended.bp <- droplevels(intended[intended$QuestionId == 'QUO3',])

untab <- aggregate(unintended.bp$Sample_Size, by = list(Response = unintended.bp$Response), FUN = sum)
tab <- aggregate(intended.bp$Sample_Size, by = list(Response = intended.bp$Response), FUN = sum)


#Question Table

unique<-unique(rd[,c('Question','QuestionId','Topic')])
#install.packages("stringr")
library(stringr)
unique$QuestionOrder<-str_split_fixed(unique$QuestionId, "QUO", 2)[,2]
#install.packages("gtools")
library(gtools)
unique<-unique[mixedorder( unique[,4] ),]
#install.packages("dplyr")
library(dplyr)
unique<-unique %>% distinct(QuestionOrder, .keep_all = TRUE)

#prop test for all questions
ids <- as.array(unique$QuestionId)
length(ids)
unintended <- rd[rd$Break_Out == 'Unintended',]
intended <- rd[rd$Break_Out == 'Intended',]
for (i in 1:length(ids)){
  #skip over two questions unintended/intended, wanted/unwanted
  if(any(ids[i]=="QUO16",ids[i]=="QUO265")){
    next
  }
  unintended.bp <- droplevels(unintended[unintended$QuestionId == ids[i],])
  intended.bp <- droplevels(intended[intended$QuestionId == ids[i],])
  
  untab <- aggregate(unintended.bp$Sample_Size, by = list(Response = unintended.bp$Response), FUN = sum)
  tab <- aggregate(intended.bp$Sample_Size, by = list(Response = intended.bp$Response), FUN = sum)
  
  x<-c(untab[2,2],tab[2,2])
  dfun<-as.data.frame(untab)
  as.numeric(dfun$x)
  df<-as.data.frame(tab)
  as.numeric(df$x)
  n<-c(sum(dfun$x),sum(df$x))
  if(length(dfun$x)>2){
    unique$response[i]<-"not tested, mult responses"
    unique$xun[i]<-"not tested, mult responses"
    unique$xin[i]<-"not tested, mult responses"
    unique$nun[i]<-"not tested, mult responses"
    unique$nin[i]<-"not tested, multresponses"
    unique$propun[i]<-"not tested, multresponses"
    unique$propin[i]<-"not tested, multresponses"
    unique$pVal[i]<-"not tested, multresponses"
    unique$significance[i]<-"not tested, multresponses"
  }else if(any(is.na(df$x[1]),is.na(dfun$x[1]))){
    unique$response[i]<-"not tested, NA"
    unique$xun[i]<-"not tested, NA"
    unique$xin[i]<-"not tested, NA"
    unique$nun[i]<-"not tested, NA"
    unique$nin[i]<-"not tested, NA"
    unique$propun[i]<-"not tested, NA"
    unique$propin[i]<-"not tested, NA"
    unique$pVal[i]<-"not tested, NA"
    unique$significance[i]<-"not tested, NA"
  } else{
    test<-prop.test(x,n)
    unique$response[i]<-df$Response[2]
    unique$xun[i]<-x[1]
    unique$xin[i]<-x[2]
    unique$nun[i]<-n[1]
    unique$nin[i]<-n[2]
    unique$propun[i]<-x[1]/n[1]
    unique$propin[i]<-x[2]/n[2]
    unique$pVal[i]<-test$p.value
    unique$significance[i]<-test$p.value<0.05
  }
}
uniqueFALSE=unique[unique$significance %in% c("FALSE"), ]
unique$diff = abs(as.numeric(unique$propun)-as.numeric(unique$propin))
unique<-unique[order(-unique$diff),]
