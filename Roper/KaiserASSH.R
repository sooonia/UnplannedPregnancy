#Some ideas about what we can do with this data: 
#Confidence intervals: Can assume normal distributions (Stat250)


library(Hmisc)
library(data.table)
library(ggplot2)
library(plyr)
kaiser98 <- spss.get("Kaiser98.por", use.value.labels=TRUE)


setnames(kaiser98, "Q46", "Race")
setnames(kaiser98, "Q44", "Education")
setnames(kaiser98, "Q29", "Current.Marriage.Status")
setnames(kaiser98, "Q42", "Sex")

Q1 <- c('Q1A', 'Q1B', 'Q1C', 'Q1D', 'Q1E')
controls <- c('USR', 'REGION', 'INCOME', 'Race', 'Education', 'Sex')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~filter by sex~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
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
  scale_fill_manual(values=c("deepskyblue3", "palevioletred1"))+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Do you feel like you would like to talk more about birth control,
talk about it enough, or don't need to talk about it?")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~filter by Region~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  scale_fill_manual(values=c("mediumvioletred","midnightblue", "gold", "mediumseagreen"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+  
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
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
  scale_fill_manual(values=c("gray49","darkseagreen3", "darkcyan"))+
  ylab('Proportion')+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 15))+
  ggtitle("Do you feel like you would like to talk more about birth control,
talk about it enough, or don't need to talk about it?")


kaiser17 <- spss.get("Kaiser17.por", use.value.labels=TRUE)