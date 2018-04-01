#~~~~~~~LOAD KAISER 98 DATA~~~~~~~~~~~

library(Hmisc)
library(data.table)
library(ggplot2)
library(plyr)
library(RWeka)
kaiser98 <- spss.get("Kaiser98.por", use.value.labels=TRUE)


setnames(kaiser98, c("Q46","Q44"), c("Race","Education"))
setnames(kaiser98, "Q29", "Current.Marriage.Status")
setnames(kaiser98, "Q42", "Sex")
setnames(kaiser98, "Q43", "Age")
kaiser98$AgeCat = cut(as.numeric(kaiser98$Age), c(-Inf,24, 34, 44, 54, 64, Inf))
levels(kaiser98$AgeCat) <- c("18-24","25-34","35-44","45-54","55-64","65+")

Q1 <- c('Q1A', 'Q1B', 'Q1C', 'Q1D', 'Q1E')
controls <- c('USR', 'REGION', 'INCOME', 'Race', 'Education', 'Sex','AgeCat')

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
td98 <- kaiser98[,new.names]

#~~~~~~~LOAD KAISER 17 DATA~~~~~~~~~~~~~~~~~~

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
td17 <- kaiser17[,c(preg.qs,controls)]

dict.td17 <- data.frame(col.name = character(), description = character())
for(i in 1:length(colnames(td17))){
  new.line = data.frame(col.name = colnames(td)[i], description= label(td17[0,i]))
  dict.td17 = rbind(dict.td17, new.line)
}

#~~~~~~~COMBINE ON AGE GROUPS~~~~~~~~~~~

td18to24 <- merge(td98[td98$AgeCat=="18-24",], td17[td17$AgeCat=="18-24",], all = TRUE)[-1]
td25to34 <- merge(td98[td98$AgeCat=="25-34",], td17[td17$AgeCat=="25-34",], all = TRUE)[-1]
td35to44 <- merge(td98[td98$AgeCat=="35-44",], td17[td17$AgeCat=="35-44",], all = TRUE)[-1]
td45to54 <- merge(td98[td98$AgeCat=="45-54",], td17[td17$AgeCat=="45-54",], all = TRUE)[-1]
td55to64 <- merge(td98[td98$AgeCat=="55-64",], td17[td17$AgeCat=="55-64",], all = TRUE)[-1]
td65 <- merge(td98[td98$AgeCat=="65+",], td17[td17$AgeCat=="65+",], all = TRUE)[-1]

#30-44 has highest gross amount unintended
#45+ and <18 have highest rates, 30-44 third highest
#look at income by age for unintended


