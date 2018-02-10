load('data.rda')

unintended <- rd[rd$Break_Out == 'Unintended',]
intended <- rd[rd$Break_Out == 'Intended',]


unintended.bp <- droplevels(unintended[unintended$QuestionId == 'QUO130',])
intended.bp <- droplevels(intended[intended$QuestionId == 'QUO130',])

untab <- aggregate(unintended.bp$Sample_Size, by = list(Response = unintended.bp$Response), FUN = sum)
tab <- aggregate(intended.bp$Sample_Size, by = list(Response = intended.bp$Response), FUN = sum)

x<-c(untab[2,2],tab[2,2])
dfun<-as.data.frame(untab)
as.numeric(dfun$x)
df<-as.data.frame(tab)
as.numeric(df$x)
n<-c(sum(dfun$x),sum(df$x))
prop.test(x,n)
    
