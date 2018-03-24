#Some ideas about what we can do with this data: 
#Confidence intervals: Can assume normal distributions (Stat250)


library(Hmisc)
library(data.table)
library(ggplot2)
library(plyr)
library(RWeka)
kaiser98 <- spss.get("Kaiser98.por", use.value.labels=TRUE)


setnames(kaiser98, c("Q46","Q44"), c("Race","Education"))
setnames(kaiser98, "Q29", "Current.Marriage.Status")
setnames(kaiser98, "Q42", "Sex")

Q1 <- c('Q1A', 'Q1B', 'Q1C', 'Q1D', 'Q1E')
controls <- c('USR', 'REGION', 'INCOME', 'Race', 'Education', 'Sex')

assoc.vars <- c(Q1, 'Q3', 'Q5B', 'Q6', 'Q7', 'Q11A', 'Q12','Q13A', 'Q13B', 
                'Q13C', 'Q14A', 'Q14B', 'Q14C', 'Q14D','Q14E', 'Q16','Q17','Q21A', 
                'Q21D', 'Q24A', 'Q27C', 'Q30','Q34', 'Q35', 'Q41B', controls)
new.names<- c('ContributionToUnplannedLackOpenness','ContributionToUnplannedPovertyEducation',
              'ContributionToUnplannedMoralValues','ContributionToUnplannedTV',
              'ContributionToUnplannedBadSexEd', 'TVTalkSafeSex','TVSexNoConsequences','HaveChildren',
              'HaveChildrenUnder18',
              'EverNotLetKidWatchTV4Sex','WhenYouthGetSexInfo','HSSexEd','JHSSexEd','ElementarySexEd',
              'HSTellKidsNoPreMaritSex','HSTellKidsUseProtection','HSTeachBasicsReproduction',
              'HSDiscussReady4Sex','HSTeachTalkSexWPartner','AbstinenceOnlyEdu','HSProvideCondoms',
            'TalkToUrKidBasicSex','TalkToUrKidCondoms','USAUptightAboutSex','HardCouplesTalkSex',
            'RelationshipStatus','HadPreMaritSex','NumSexPartners','TalkAbtBirthControl',controls)
setnames(kaiser98, assoc.vars, new.names)

ab.only <- kaiser98[kaiser98$AbstinenceOnlyEdu == "Only abstinence",new.names]
Apriori(ab.only[,names(ab.only) != 'AbstinenceOnlyEdu'], control = Weka_control(N = 30))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~filter by sex~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col.list <- c("deepskyblue3", "palevioletred1")
#Q1B
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q1B))),
                        Response=as.factor(names(table(Q1B))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of poverty and poor education \nto unplanned pregnancies and STDs Scaled")

#Q1C
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q1C))),
                        Response=as.factor(names(table(Q1C))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of decline of moral values \nto unplanned pregnancies and STDs Scaled")

#Q1E
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
             prop=as.numeric(prop.table(table(Q1E))),
             Response=as.factor(names(table(Q1E))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)
                                      
ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of inadequate sex ed \nto unplanned pregnancies and STDs Scaled")

#Q13A
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q13A))),
                        Response=as.factor(names(table(Q13A))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n high school students (15-18)")

#Q13B
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q13B))),
                        Response=as.factor(names(table(Q13B))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n junior high school students (12-14)")

#Q13C
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q13C))),
                        Response=as.factor(names(table(Q13C))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n elementary students (6-12)")

#Q16
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q16))),
                        Response=as.factor(names(table(Q16))))
order <- c("Both", 
           "Only abstinence", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you think sex ed should teach only abstinence\n or both safe sex and abstinence")

#Q17
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q17))),
                        Response=as.factor(names(table(Q17))))
order <- c("Yes", 
           "No", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you think high school health clinics should provide
young people with condoms and other forms of birth control")

#Q27C
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q27C))),
                        Response=as.factor(names(table(Q27C))))
order <- c("Strongly agree", 
           "Somewhat agree", 
           "Somewhat disagree",
           "Strongly disagree",
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Sometimes it's harder for couples to talk about birth control
and STD's than to have sex")

#Q41B
kaiser98.scaled <-ddply(kaiser98,.(Sex),summarise,
                        prop=as.numeric(prop.table(table(Q41B))),
                        Response=as.factor(names(table(Q41B))))
order <- c("More", 
           "Enough", 
           "Don't need to talk about",
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=Sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Do you feel like you would like to talk more about birth control,
talk about it enough, or don't need to talk about it?")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~filter by Region~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col.list <- c("mediumvioletred","midnightblue", "gold", "mediumseagreen")
#Q1B
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q1B))),
                        Response=as.factor(names(table(Q1B))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of poverty and poor education \nto unplanned pregnancies and STDs Scaled")

#Q1C
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q1C))),
                        Response=as.factor(names(table(Q1C))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of decline of moral values \nto unplanned pregnancies and STDs Scaled")

#Q1E
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q1E))),
                        Response=as.factor(names(table(Q1E))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of inadequate sex ed \nto unplanned pregnancies and STDs Scaled")

#Q13A
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q13A))),
                        Response=as.factor(names(table(Q13A))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n high school students (15-18)")

#Q13B
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q13B))),
                        Response=as.factor(names(table(Q13B))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n junior high school students (12-14)")

#Q13C
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q13C))),
                        Response=as.factor(names(table(Q13C))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n elementary students (6-12)")

#Q16
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q16))),
                        Response=as.factor(names(table(Q16))))
order <- c("Both", 
           "Only abstinence", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you think sex ed should teach only abstinence\n or both safe sex and abstinence")

#Q17
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q17))),
                        Response=as.factor(names(table(Q17))))
order <- c("Yes", 
           "No", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you think high school health clinics should provide
          young people with condoms and other forms of birth control")

#Q27C
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q27C))),
                        Response=as.factor(names(table(Q27C))))
order <- c("Strongly agree", 
           "Somewhat agree", 
           "Somewhat disagree",
           "Strongly disagree",
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Sometimes it's harder for couples to talk about birth control
          and STD's than to have sex")

#Q41B
kaiser98.scaled <-ddply(kaiser98,.(REGION),summarise,
                        prop=as.numeric(prop.table(table(Q41B))),
                        Response=as.factor(names(table(Q41B))))
order <- c("More", 
           "Enough", 
           "Don't need to talk about",
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=REGION)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Do you feel like you would like to talk more about birth control,
          talk about it enough, or don't need to talk about it?")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~filter by USR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col.list <- c("gray49","darkseagreen3", "darkcyan")
#Q1B
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q1B))),
                        Response=as.factor(names(table(Q1B))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of poverty and poor education \nto unplanned pregnancies and STDs Scaled")

#Q1C
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q1C))),
                        Response=as.factor(names(table(Q1C))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of decline of moral values \nto unplanned pregnancies and STDs Scaled")

#Q1E
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q1E))),
                        Response=as.factor(names(table(Q1E))))
order <- c("A lot", 
           "Some", 
           "Only a little", 
           "Not at all", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(kaiser98.scaled$Response, order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Contribution of inadequate sex ed \nto unplanned pregnancies and STDs Scaled")

#Q13A
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q13A))),
                        Response=as.factor(names(table(Q13A))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+  
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n high school students (15-18)")

#Q13B
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q13B))),
                        Response=as.factor(names(table(Q13B))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n junior high school students (12-14)")

#Q13C
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q13C))),
                        Response=as.factor(names(table(Q13C))))
order <- c("Support", 
           "Oppose", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you support sex ed taught to\n elementary students (6-12)")

#Q16
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q16))),
                        Response=as.factor(names(table(Q16))))
order <- c("Both", 
           "Only abstinence", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you think sex ed should teach only abstinence\n or both safe sex and abstinence")

#Q17
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q17))),
                        Response=as.factor(names(table(Q17))))
order <- c("Yes", 
           "No", 
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  ggtitle("Do you think high school health clinics should provide
young people with condoms and other forms of birth control")

#Q27C
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q27C))),
                        Response=as.factor(names(table(Q27C))))
order <- c("Strongly agree", 
           "Somewhat agree", 
           "Somewhat disagree",
           "Strongly disagree",
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Sometimes it's harder for couples to talk about birth control
and STD's than to have sex")

#Q41B
kaiser98.scaled <-ddply(kaiser98,.(USR),summarise,
                        prop=as.numeric(prop.table(table(Q41B))),
                        Response=as.factor(names(table(Q41B))))
order <- c("More", 
           "Enough", 
           "Don't need to talk about",
           "Don't know",
           "Refused")
kaiser98.scaled$Response <- factor(as.factor(kaiser98.scaled$Response), order)

ggplot(data=kaiser98.scaled, aes(x=Response, y=prop, fill=USR)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Do you feel like you would like to talk more about birth control,
talk about it enough, or don't need to talk about it?")


















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~KAISER 2017~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

kaiser17 <- spss.get("Kaiser17.por", use.value.labels=TRUE)
setnames(kaiser17, "EDUC2", "Education")
setnames(kaiser17, "QD14", "Income")
setnames(kaiser17, "RECAGE", "AgeCat")
setnames(kaiser17, "QD8B", "PoliticsGeneral")
setnames(kaiser17, "QD2B", "Married")
setnames(kaiser17, "PARTY5", "PoliticalPartyAffiliation")

dict <- data.frame(col.name = character(), description = character())
for(i in 1:length(colnames(kaiser17))){
  new.line = data.frame(col.name = colnames(kaiser17)[i], description= label(kaiser17[0,i]))
  dict = rbind(dict, new.line)
}

preg.qs <- c('Q13C','Q18AA', 'Q18AB', 'Q20', 'Q21', 'Q22', 'Q23A', 'Q23B', 'Q23C', 'Q23D',
             'Q24', 'Q25', 'Q26', 'Q27', 'Q29', 'Q29A')
controls <- c('Education', "AgeCat", "PoliticalPartyAffiliation", 'Income', 'PoliticsGeneral', 'Married')
td <- kaiser17[,c(preg.qs,controls)]

dict.td <- data.frame(col.name = character(), description = character())
for(i in 1:length(colnames(td))){
  new.line = data.frame(col.name = colnames(td)[i], description= label(td[0,i]))
  dict.td = rbind(dict.td, new.line)
}


col.list <- c('blue', 'deepskyblue', 'green4', 'indianred1', 'red2', 'lavenderblush4')
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")

levels(td$Q18AA)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                        prop=as.numeric(prop.table(table(Q18AA))),
                        Response=as.factor(names(table(Q18AA))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Not at all important",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("How important for private health plans to cover birth control?")


levels(td$Q22)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Should not be done",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=col.list)+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("How important for fed gov to provide funding for \nreproductive services?")


#-------INCOME-----------------

#how does income relate to political affiliation?
plot(table(td$PoliticalPartyAffiliation,td$Income))

# Qualitative color schemes by Paul Tol
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

c('Q13C','Q18AA', 'Q18AB', 'Q20', 'Q21', 'Q22', 'Q23A', 'Q23B', 'Q23C', 'Q23D',
  'Q24', 'Q25', 'Q26', 'Q27', 'Q29', 'Q29A')

#   ---Q13C---
levels(td$Q13C)[3:4] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q13C))),
                  Response=as.factor(names(table(Q13C))))
order <- c("Yes, the law does this" , 
           "No, law does not do this", 
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Does 2010 law eliminate birth control costs for women?")

#   ---Q21---
levels(td$Q21)[1:6] <- c("Government","Insurance Company","Woman Herself","None of these","Don't know","Refused")
td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q21))),
                  Response=as.factor(names(table(Q21))))
order <- c("Government",
           "Insurance Company",
           "Woman Herself",
           "None of these",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Who should pay for birth control in case of company religious exception?")

#   ---Q23A---
#   ---Q23B---
#   ---Q23C---
#   ---Q23D---
#   ---Q24---
#   ---Q25---
#   ---Q26---
#   ---Q27--
#   ---Q29---
#   ---Q29A---

#   ---Q18AA---

levels(td$Q18AA)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q18AA))),
                  Response=as.factor(names(table(Q18AA))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Should not be done",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important private companies pay birth control no expense to women?")

#   ---Q18AB---

levels(td$Q18AB)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q18AB))),
                  Response=as.factor(names(table(Q18AB))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Should not be done",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover pregnant women?")

#   ---Q20---

levels(td$Q20)[3:4] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q20))),
                  Response=as.factor(names(table(Q20))))
order <- c("Support" , 
           "Oppose", 
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("View on requirement private companies cover full cost birth control?")

#   ---Q22---

levels(td$Q22)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Should not be done",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for fed gov to provide funding for \nreproductive services?")

