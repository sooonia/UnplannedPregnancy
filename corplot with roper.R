library(Hmisc)
library(data.table)
library(ggplot2)
library(plyr)
library(RWeka)
kaiser98 <- spss.get("Kaiser98.por")
cordata <- kaiser98[,c('Q1A','Q1B','Q1C','Q1D','Q1E', 'Q3', 'Q5B', 'Q6', 'Q7', 'Q11A', 'Q12','Q13A', 'Q13B', 
                       'Q13C', 'Q14A', 'Q14B', 'Q14C', 'Q14D','Q14E', 'Q16','Q17','Q21A', 
                       'Q21D', 'Q24A', 'Q27C', 'Q30','Q34', 'Q35', 'Q41B')]
new.names<- c('ContributionToUnplannedLackOpenness','ContributionToUnplannedPovertyEducation',
              'ContributionToUnplannedMoralValues','ContributionToUnplannedTV',
              'ContributionToUnplannedBadSexEd', 'TVTalkSafeSex','TVSexNoConsequences','HaveChildren',
              'HaveChildrenUnder18',
              'EverNotLetKidWatchTV4Sex','WhenYouthGetSexInfo','HSSexEd','JHSSexEd','ElementarySexEd',
              'HSTellKidsNoPreMaritSex','HSTellKidsUseProtection','HSTeachBasicsReproduction',
              'HSDiscussReady4Sex','HSTeachTalkSexWPartner','AbstinenceOnlyEdu','HSProvideCondoms',
              'TalkToUrKidBasicSex','TalkToUrKidCondoms','USAUptightAboutSex','HardCouplesTalkSex',
              'RelationshipStatus','HadPreMaritSex','NumSexPartners','TalkAbtBirthControl')
write.csv(cordata,'cordata.csv')
##-----did things in excel, sorry, not transparent, not smart but got frusterated----###
cordata2<-as.data.frame(read.csv('cordata.csv'))
cordata2<-cordata2[,2:30]
#update values to see if can see more correlation
for(col in colnames(cordata2)){
  cordata2[cordata2[,col]==1, col]  <- 8
  cordata2[cordata2[,col]==2, col]  <- 10
  cordata2[cordata2[,col]==-1, col]  <- -8
  cordata2[cordata2[,col]==-2, col]  <- -10
}
#update sexual partners to median
cordata2[cordata2$Q35==0.5,'Q35']<-1
cordata2[cordata2$Q35==8,'Q35']<-2
cordata2[cordata2$Q35==10,'Q35']<-4.5
cordata2[cordata2$Q35==3,'Q35']<-8.5
cordata2[cordata2$Q35==4,'Q35']<-15.5
cordata2[cordata2$Q35==5,'Q35']<-30

library(corrplot)
corr<-cor(cordata2)
rownames(corr)=new.names
colnames(corr)=new.names

#colors for correlation plot
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")


corrplot(M, order = "hclust", addrect = 2, col = col4(10))
corrplot(corr,method="square", tl.cex=0.55,tl.col="black",order="hclust", col=col4(10),tl.srt=35,type="lower")
