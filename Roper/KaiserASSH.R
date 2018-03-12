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









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~KAISER 2017/ J U N K~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

kaiser17 <- spss.get("Kaiser17.por", use.value.labels=TRUE)
setnames(kaiser17, "EDUC2", "Education")
setnames(kaiser17, "QD14", "Income")
setnames(kaiser17, "RECAGE", "AgeCat")
setnames(kaiser17, "QD8B", "PoliticsGeneral")
setnames(kaiser17, "QD2B", "Married")
setnames(kaiser17, "PARTY5", "PoliticalPartyAffiliation")

controls <- c('Education', "AgeCat", "PoliticalAffiliation")
