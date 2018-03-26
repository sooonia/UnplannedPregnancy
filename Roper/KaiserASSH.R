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


col.list <- c('#4477AA', 'deepskyblue', 'palegreen3', '#CC6677', 'firebrick', 'lavenderblush4')
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
tol4qualitative=c("#4477AA", "palegreen3", "#CC6677", "lavenderblush4")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

setnames(kaiser17, "EDUC2", "Education")
setnames(kaiser17, "QD14", "Income")
setnames(kaiser17, "RECAGE", "AgeCat")
setnames(kaiser17, "QD8B", "PoliticsGeneral")
setnames(kaiser17, "QD2B", "Married")
setnames(kaiser17, "PARTY5", "PoliticalPartyAffiliation")


levels(td$PoliticsGeneral)[4] <- "Don't know/Refused"
levels(td$Income)[9] <- 'Don\'t know/Refused'
levels(td$Education) <- c("< HS", "HS incomplete", "HS", "Some College", 
                          "2 year associate degree", "4 year college", "some postgrad",
                          "postgrad +", "Don't know/Refused")

#   ---Q13C---

#education

levels(td$Q13C)[3:4] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q13C))),
                  Response=as.factor(names(table(Q13C))))
order <- c("Yes, the law does this" , 
           "No, law does not do this", 
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Does 2010 law eliminate birth control costs for women?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q13C))),
                  Response=as.factor(names(table(Q13C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Does 2010 law eliminate birth control costs for women?")


#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q13C))),
                  Response=as.factor(names(table(Q13C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Does 2010 law eliminate birth control costs for women?")


#politics general

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q13C))),
                  Response=as.factor(names(table(Q13C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Does 2010 law eliminate birth control costs for women?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q13C))),
                  Response=as.factor(names(table(Q13C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Does 2010 law eliminate birth control costs for women?")

#politicalPartyAffiliation

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q13C))),
                  Response=as.factor(names(table(Q13C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Does 2010 law eliminate birth control costs for women?")



#   ---Q18AA---
#education

levels(td$Q18AA)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q18AA))),
                  Response=as.factor(names(table(Q18AA))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Should not be done",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important private companies pay birth control no expense to women?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q18AA))),
                  Response=as.factor(names(table(Q18AA))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)
ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important private companies pay birth control no expense to women?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q18AA))),
                  Response=as.factor(names(table(Q18AA))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)
ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important private companies pay birth control no expense to women?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q18AA))),
                  Response=as.factor(names(table(Q18AA))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)
ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover birth control?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q18AA))),
                  Response=as.factor(names(table(Q18AA))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)
ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important private companies pay birth control no expense to women?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q18AA))),
                  Response=as.factor(names(table(Q18AA))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)
ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important private companies pay birth control no expense to women?")

#   ---Q18AB---
#education

levels(td$Q18AB)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q18AB))),
                  Response=as.factor(names(table(Q18AB))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Should not be done",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover pregnant women?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q18AB))),
                  Response=as.factor(names(table(Q18AB))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover pregnant women?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q18AB))),
                  Response=as.factor(names(table(Q18AB))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover pregnant women?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q18AB))),
                  Response=as.factor(names(table(Q18AB))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover pregnant women?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q18AB))),
                  Response=as.factor(names(table(Q18AB))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover pregnant women?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q18AB))),
                  Response=as.factor(names(table(Q18AB))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for private health plans to cover pregnant women?")


#   ---Q20---
#education

levels(td$Q20)[3:4] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q20))),
                  Response=as.factor(names(table(Q20))))
order <- c("Support" , 
           "Oppose", 
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("View on requirement private companies cover full cost birth control?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q20))),
                  Response=as.factor(names(table(Q20))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("View on requirement private companies cover full cost birth control?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q20))),
                  Response=as.factor(names(table(Q20))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("View on requirement private companies cover full cost birth control?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q20))),
                  Response=as.factor(names(table(Q20))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("View on requirement private companies cover full cost birth control?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q20))),
                  Response=as.factor(names(table(Q20))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("View on requirement private companies cover full cost birth control?")

#politicalpartyaffliation

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q20))),
                  Response=as.factor(names(table(Q20))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("View on requirement private companies cover full cost birth control?")


#   ---Q21---
#education

levels(td$Q21)[1:6] <- c("Government","Insurance Company","Woman Herself","None of these","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q21))),
                  Response=as.factor(names(table(Q21))))
order <- c("Government",
           "Insurance Company",
           "Woman Herself",
           "None of these",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Who should pay for birth control in case of company religious exception?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q21))),
                  Response=as.factor(names(table(Q21))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Who should pay for birth control in case of company religious exception?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q21))),
                  Response=as.factor(names(table(Q21))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Who should pay for birth control in case of company religious exception?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q21))),
                  Response=as.factor(names(table(Q21))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Who should pay for birth control in case of company religious exception?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q21))),
                  Response=as.factor(names(table(Q21))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Who should pay for birth control in case of company religious exception?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q21))),
                  Response=as.factor(names(table(Q21))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Who should pay for birth control in case of company religious exception?")


#   ---Q22---
#education

levels(td$Q22)[5:6] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
order <- c("Very important" , 
           "Somewhat important", 
           "Not too important",
           "Should not be done",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for fed gov to provide funding for \nreproductive services?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for fed gov to provide funding for \nreproductive services?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for fed gov to provide funding for \nreproductive services?")

#polgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for fed gov to provide funding for \nreproductive services?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for fed gov to provide funding for \nreproductive services?")

#politicalpartyaffiliation

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q22))),
                  Response=as.factor(names(table(Q22))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("How important for fed gov to provide funding for \nreproductive services?")


#   ---Q23A---
#education

levels(td$Q23A)[1:6] <- c("Yes","No","Never heard of PP","Skipped","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))
order <- c("Yes",
           "No",
           "Never heard of PP",
           "Skipped",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide abortions?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide abortions?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide abortions?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide abortions?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide abortions?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q23A))),
                  Response=as.factor(names(table(Q23A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide abortions?")

#   ---Q23B---
#education

levels(td$Q23B)[1:6] <- c("Yes","No","Never heard of PP","Skipped","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q23B))),
                  Response=as.factor(names(table(Q23B))))
order <- c("Yes",
           "No",
           "Never heard of PP",
           "Skipped",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide testing and treatment for STIs?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q23B))),
                  Response=as.factor(names(table(Q23B))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide testing and treatment for STIs?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q23B))),
                  Response=as.factor(names(table(Q23B))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide testing and treatment for STIs?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q23B))),
                  Response=as.factor(names(table(Q23B))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide testing and treatment for STIs?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q23B))),
                  Response=as.factor(names(table(Q23B))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide testing and treatment for STIs?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q23B))),
                  Response=as.factor(names(table(Q23B))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide testing and treatment for STIs?")

#   ---Q23C---
#education

levels(td$Q23C)[1:6] <- c("Yes","No","Never heard of PP","Skipped","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q23C))),
                  Response=as.factor(names(table(Q23C))))
order <- c("Yes",
           "No",
           "Never heard of PP",
           "Skipped",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide cancer screenings and preventative services?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q23C))),
                  Response=as.factor(names(table(Q23C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide cancer screenings and preventative services?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q23C))),
                  Response=as.factor(names(table(Q23C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide cancer screenings and preventative services?")

#polticsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q23C))),
                  Response=as.factor(names(table(Q23C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide cancer screenings and preventative services?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q23C))),
                  Response=as.factor(names(table(Q23C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide cancer screenings and preventative services?")

#politicalpartyaffiliation

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q23C))),
                  Response=as.factor(names(table(Q23C))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide cancer screenings and preventative services?")

#   ---Q23D---
#education

levels(td$Q23D)[1:6] <- c("Yes","No","Never heard of PP","Skipped","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q23D))),
                  Response=as.factor(names(table(Q23D))))
order <- c("Yes",
           "No",
           "Never heard of PP",
           "Skipped",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide contraception, including birth control?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q23D))),
                  Response=as.factor(names(table(Q23D))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide contraception, including birth control?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q23D))),
                  Response=as.factor(names(table(Q23D))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide contraception, including birth control?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q23D))),
                  Response=as.factor(names(table(Q23D))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide contraception, including birth control?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q23D))),
                  Response=as.factor(names(table(Q23D))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide contraception, including birth control?")

#politicalparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q23D))),
                  Response=as.factor(names(table(Q23D))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("To the best of your knowledge, does Planned Parenthood provide contraception, including birth control?")


#   ---Q24---
#education

levels(td$Q24)[1:4] <- c("Yes","No","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q24))),
                  Response=as.factor(names(table(Q24))))
order <- c("Yes" , 
           "No", 
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("As far as you know, is there a ban on federal Medicaid \nfunds being used to pay for abortions, or not?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q24))),
                  Response=as.factor(names(table(Q24))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("As far as you know, is there a ban on federal Medicaid \nfunds being used to pay for abortions, or not?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q24))),
                  Response=as.factor(names(table(Q24))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("As far as you know, is there a ban on federal Medicaid funds being used to pay for abortions, or not?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q24))),
                  Response=as.factor(names(table(Q24))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("As far as you know, is there a ban on federal Medicaid funds being used to pay for abortions, or not?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q24))),
                  Response=as.factor(names(table(Q24))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("As far as you know, is there a ban on federal Medicaid funds being used to pay for abortions, or not?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q24))),
                  Response=as.factor(names(table(Q24))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("As far as you know, is there a ban on federal Medicaid funds being used to pay for abortions, or not?")

#   ---Q25---
#education

levels(td$Q25)[1:5] <- c("Continue","Stop all","Pay abortion too (not read)","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q25))),
                  Response=as.factor(names(table(Q25))))
order <- c("Continue",
           "Stop all",
           "Pay abortion too (not read)",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Should Medicaid continue to pay Planned Parenthood for non-abortion services, or stop all payment?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q25))),
                  Response=as.factor(names(table(Q25))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Should Medicaid continue to pay Planned Parenthood for non-abortion services, or stop all payment?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q25))),
                  Response=as.factor(names(table(Q25))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Should Medicaid continue to pay Planned Parenthood for non-abortion services, or stop all payment?")

#polgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q25))),
                  Response=as.factor(names(table(Q25))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Should Medicaid continue to pay Planned Parenthood for non-abortion services, or stop all payment?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q25))),
                  Response=as.factor(names(table(Q25))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Should Medicaid continue to pay Planned Parenthood for non-abortion services, or stop all payment?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q25))),
                  Response=as.factor(names(table(Q25))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Should Medicaid continue to pay Planned Parenthood for non-abortion services, or stop all payment?")

#   ---Q26---
#education

levels(td$Q26)[1:4] <- c("Still stop all","Now want to keep payment for non-abortion","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q26))),
                  Response=as.factor(names(table(Q26))))
order <- c("Still stop all",
           "Now want to keep payment for non-abortion",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that cutting off payment to PP would make it difficult for many lower income women to obtain certain health services?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q26))),
                  Response=as.factor(names(table(Q26))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that cutting off payment to PP would make it difficult for many lower income women to obtain certain health services?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q26))),
                  Response=as.factor(names(table(Q26))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that cutting off payment to PP would make it difficult for many lower income women to obtain certain health services?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q26))),
                  Response=as.factor(names(table(Q26))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that cutting off payment to PP would make it difficult for many lower income women to obtain certain health services?")

#marrried

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q26))),
                  Response=as.factor(names(table(Q26))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that cutting off payment to PP would make it difficult for many lower income women to obtain certain health services?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q26))),
                  Response=as.factor(names(table(Q26))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that cutting off payment to PP would make it difficult for many lower income women to obtain certain health services?")

#   ---Q27--

#education

levels(td$Q27)[1:4] <- c("Still keep paying","Now want to stop all","Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q27))),
                  Response=as.factor(names(table(Q27))))
order <- c("Still keep paying",
           "Now want to stop all",
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that even though the government does not provide any funding to PP for abortions, PP does provide and refer abortions?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q27))),
                  Response=as.factor(names(table(Q27))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that even though the government does not provide any funding to PP for abortions, PP does provide and refer abortions?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q27))),
                  Response=as.factor(names(table(Q27))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that even though the government does not provide any funding to PP for abortions, PP does provide and refer abortions?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q27))),
                  Response=as.factor(names(table(Q27))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that even though the government does not provide any funding to PP for abortions, PP does provide and refer abortions?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q27))),
                  Response=as.factor(names(table(Q27))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that even though the government does not provide any funding to PP for abortions, PP does provide and refer abortions?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q27))),
                  Response=as.factor(names(table(Q27))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard that even though the government does not provide any funding to PP for abortions, PP does provide and refer abortions?")

#   ---Q29---

#education

levels(td$Q29)[3:4] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q29))),
                  Response=as.factor(names(table(Q29))))
order <- c("Support" , 
           "Oppose", 
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Requirement for private health care to include coverage for maternity services?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q29))),
                  Response=as.factor(names(table(Q29))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Requirement for private health care to include coverage for maternity services?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q29))),
                  Response=as.factor(names(table(Q29))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Requirement for private health care to include coverage for maternity services?")

#politicsgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q29))),
                  Response=as.factor(names(table(Q29))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Requirement for private health care to include coverage for maternity services?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q29))),
                  Response=as.factor(names(table(Q29))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Requirement for private health care to include coverage for maternity services?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q29))),
                  Response=as.factor(names(table(Q29))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("Requirement for private health care to include coverage for maternity services?")


#   ---Q29A---

#education

levels(td$Q29A)[3:4] <- c("Don't know","Refused")
td.scaled <-ddply(td,.(Education),summarise,
                  prop=as.numeric(prop.table(table(Q29A))),
                  Response=as.factor(names(table(Q29A))))
order <- c("Still support" , 
           "Now oppose", 
           "Don't know",
           "Refused")
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Education)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard this requirement means some people pay for benefits they don't use?")

#income

td.scaled <-ddply(td,.(Income),summarise,
                  prop=as.numeric(prop.table(table(Q29A))),
                  Response=as.factor(names(table(Q29A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Income)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol9qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard this requirement means some people pay for benefits they don't use?")

#agecat

td.scaled <-ddply(td,.(AgeCat),summarise,
                  prop=as.numeric(prop.table(table(Q29A))),
                  Response=as.factor(names(table(Q29A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=AgeCat)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard this requirement means some people pay for benefits they don't use?")

#polgeneral

td.scaled <-ddply(td,.(PoliticsGeneral),summarise,
                  prop=as.numeric(prop.table(table(Q29A))),
                  Response=as.factor(names(table(Q29A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticsGeneral)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol4qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard this requirement means some people pay for benefits they don't use?")

#married

td.scaled <-ddply(td,.(Married),summarise,
                  prop=as.numeric(prop.table(table(Q29A))),
                  Response=as.factor(names(table(Q29A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=Married)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol7qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard this requirement means some people pay for benefits they don't use?")

#polparty

td.scaled <-ddply(td,.(PoliticalPartyAffiliation),summarise,
                  prop=as.numeric(prop.table(table(Q29A))),
                  Response=as.factor(names(table(Q29A))))
td.scaled$Response <- factor(as.factor(td.scaled$Response), order)

ggplot(data=td.scaled, aes(x=Response, y=prop, fill=PoliticalPartyAffiliation)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values=tol6qualitative)+
    ylab('Proportion')+
    ylim(0,1)+
    theme(axis.text.x = element_text(angle = 15))+
    ggtitle("What if you heard this requirement means some people pay for benefits they don't use?")

