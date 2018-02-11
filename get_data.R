load('data.rda')

unintended <- rd[rd$Break_Out == 'Unintended',]
intended <- rd[rd$Break_Out == 'Intended',]


unintended.bp <- droplevels(unintended[unintended$QuestionId == 'QUO1',])
intended.bp <- droplevels(intended[intended$QuestionId == 'QUO130',])

untab <- aggregate(unintended.bp$Sample_Size, by = list(Response = unintended.bp$Response), FUN = sum)
tab <- aggregate(intended.bp$Sample_Size, by = list(Response = intended.bp$Response), FUN = sum)

x<-c(untab[2,2],tab[2,2])
dfun<-as.data.frame(untab)
as.numeric(dfun$x)
df<-as.data.frame(tab)
as.numeric(df$x)
n<-c(sum(dfun$x),sum(df$x))
test<-prop.test(x,n)
pvalue<-test$p.value

#Question Table
load('data.rda')
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


