library(Hmisc)
library(data.table)
library(ggplot2)
library(plyr)
library(viridis)

kaiser17 <- spss.get("Roper/Kaiser17.por", use.value.labels=TRUE)
setnames(kaiser17, "EDUC2", "Education")
setnames(kaiser17, "QD14", "Income")
setnames(kaiser17, "RECAGE", "AgeCat")
setnames(kaiser17, "QD8B", "PoliticsGeneral")
setnames(kaiser17, "QD2B", "Married")
setnames(kaiser17, "PARTY5", "PoliticalPartyAffiliation")

preg.qs <- c('Q13C','Q18AA', 'Q18AB', 'Q20', 'Q21', 'Q22', 'Q23A', 'Q23B', 'Q23C', 'Q23D',
             'Q24', 'Q25', 'Q26', 'Q27', 'Q29', 'Q29A')

kaiser17$med.income <- 0
kaiser17[kaiser17$Income == 'Less than $20,000','med.income'] <- 10000
kaiser17[kaiser17$Income == '$20,000 to less than $30,000','med.income'] <- 25000
kaiser17[kaiser17$Income == '$30,000 to less than $40,000','med.income'] <- 35000
kaiser17[kaiser17$Income == '$40,000 to less than $50,000','med.income'] <- 45000
kaiser17[kaiser17$Income == '$50,000 to less than $75,000','med.income'] <- 62500
kaiser17[kaiser17$med.income == 0,'med.income'] <- 100000

dict <- data.frame(col.name = character(), description = character())
for(i in 1:length(colnames(kaiser17))){
  new.line = data.frame(col.name = colnames(kaiser17)[i], description= label(kaiser17[0,i]))
  dict = rbind(dict, new.line)
}

td <- kaiser17[kaiser17$Income != '(DO NOT READ) Don\'t know/Refused',]
td <- td[td$SEX == 'Female',]

levels(td$Q23A) <- c('Yes', 'No', 'NA', 'NA', 'NA', 'NA' )
levels(td$Q23D) <- c('Yes', 'No', 'NA', 'NA', 'NA', 'NA' )
levels(td$Q24) <- c('Yes', 'No', 'NA', 'NA')
levels(td$Q29) <- c('Support', 'Oppose', 'NA', 'NA')
levels(td$Q29A) <- c('Still support', 'Now oppose', 'NA', 'NA')


tol8qualitative=c("pink", "pink1", "pink2", "palevioletred1", "palevioletred2",
                  "violetred2", "violetred3","violetred4")
#tol8qualitative=rev(viridis(8))

# -----------------------------------   Q23A  -------------------------------------------------------

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))

med.scaled <-ddply(td,.(med.income),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))

corr <- med.scaled
corr$med.income <- as.numeric(corr$med.income)
cor(corr[corr$Response == 'Yes', 'med.income'],corr[corr$Response == 'Yes', 'prop'])
cor(corr[corr$Response == 'No', 'med.income'],corr[corr$Response == 'No', 'prop'])

order <- c("Yes" , 
           "No", 
           "NA")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

levels(td.scaled$Response) <- c( "Yes (Correct)", "No", "NA" )

ggplot(data=td.scaled[td.scaled$Response != 'NA',], aes(x=Response, y=prop, fill=Income)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("To the best of your knowledge, does PP provide abortions?")

ggplot(data=td[td$Q23A != 'NA',], aes(x=Q23A, fill=Income)) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Total')+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("To the best of your knowledge, does PP provide abortions?")



# -----------------------------------   Q23D  -------------------------------------------------------

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q23D))),
                  Response=as.factor(names(table(Q23D))))

med.scaled <-ddply(td,.(med.income),summarise,
                   prop=as.numeric(prop.table(table(Q23D))),
                   Response=as.factor(names(table(Q23D))))

corr <- med.scaled
corr$med.income <- as.numeric(corr$med.income)
cor(corr[corr$Response == 'Yes', 'med.income'],corr[corr$Response == 'Yes', 'prop'])
cor(corr[corr$Response == 'No', 'med.income'],corr[corr$Response == 'No', 'prop'])

order <- c("Yes" , 
           "No", 
           "NA")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled[td.scaled$Response != 'NA',], aes(x=Response, y=prop, fill=Income)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("To the best of your knowledge, does PP provide contraception?")

ggplot(data=td[td$Q23D != 'NA',], aes(x=Q23D, fill=Income)) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Total')+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("To the best of your knowledge, does PP provide contracption?")



# -----------------------------------   Q24  -------------------------------------------------------

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q24))),
                  Response=as.factor(names(table(Q24))))

med.scaled <-ddply(td,.(med.income),summarise,
                   prop=as.numeric(prop.table(table(Q24))),
                   Response=as.factor(names(table(Q24))))

corr <- med.scaled
corr$med.income <- as.numeric(corr$med.income)
cor(corr[corr$Response == 'Yes', 'med.income'],corr[corr$Response == 'Yes', 'prop'])
cor(corr[corr$Response == 'No', 'med.income'],corr[corr$Response == 'No', 'prop'])

order <- c("Yes" , 
           "No", 
           "NA")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

levels(td.scaled$Response) <- c("Yes (Correct)", "No", "NA" )

ggplot(data=td.scaled[td.scaled$Response != 'NA',], aes(x=Response, y=prop, fill=Income)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("To the best of your knowledge, is there a ban on \nfederal medicaid funds being used to pay for abortions?")

ggplot(data=td[td$Q23D != 'NA',], aes(x=Q23D, fill=Income)) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Total')+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("To the best of your knowledge, is there a ban on \nfederal medicaid funds being used to pay for abortions?")




# -----------------------------------   Q29  -------------------------------------------------------

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q29))),
                  Response=as.factor(names(table(Q29))))

med.scaled <-ddply(td,.(med.income),summarise,
                   prop=as.numeric(prop.table(table(Q29))),
                   Response=as.factor(names(table(Q29))))

corr <- med.scaled
corr$med.income <- as.numeric(corr$med.income)
cor(corr[corr$Response == 'Support', 'med.income'],corr[corr$Response == 'Support', 'prop'])
cor(corr[corr$Response == 'Oppose', 'med.income'],corr[corr$Response == 'Oppose', 'prop'])

order <- c("Support" , 
           "Oppose", 
           "NA")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled[td.scaled$Response %in% c('Oppose', 'Support'),], aes(x=Response, y=prop, fill=Income)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Current health care law requires that all private health \nplans must include coverage for maternity care. \nIn general, do you support or oppose this requirement?")

ggplot(data=td[td$Q23D != 'NA',], aes(x=Q23D, fill=Income)) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Total')+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Current health care law requires that all private health \nplans must include coverage for maternity care. \nIn general, do you support or oppose this requirement?")




# -----------------------------------   Q29A  -------------------------------------------------------

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q29A))),
                  Response=as.factor(names(table(Q29A))))

med.scaled <-ddply(td,.(med.income),summarise,
                   prop=as.numeric(prop.table(table(Q29A))),
                   Response=as.factor(names(table(Q29A))))

corr <- med.scaled
corr$med.income <- as.numeric(corr$med.income)
cor(corr[corr$Response == 'Still support', 'med.income'],corr[corr$Response == 'Still support', 'prop'])
cor(corr[corr$Response == 'Now oppose', 'med.income'],corr[corr$Response == 'Now oppose', 'prop'])

order <- c("Still support" , 
           "Now oppose", 
           "NA")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled[td.scaled$Response %in% c('Now oppose', 'Still support'),], aes(x=Response, y=prop, fill=Income)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("What if you heard that the requirement for all private health \nplans to include coverage for maternity care means some \npeople have to pay for benefits they do not use? \nDo you still support the requirement?")

ggplot(data=td[td$Q23D != 'NA',], aes(x=Q23D, fill=Income)) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_fill_manual(values=tol8qualitative)+
  ylab('Total')+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("What if you heard that the requirement for all private health \nplans to include coverage for maternity care means some \npeople have to pay for benefits they do not use? \nDo you still support the requirement?")
