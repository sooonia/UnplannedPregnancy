library(Hmisc)
library(data.table)
library(ggplot2)
library(plyr)
library(RWeka)
kaiser98 <- spss.get("Kaiser98.por")
cordata <- kaiser98[,c('Q1A','Q1B','Q1C','Q1D','Q1E', 'Q3', 'Q5B', 'Q6', 'Q7', 'Q11A', 'Q12','Q13A', 'Q13B', 
                       'Q13C', 'Q14A', 'Q14B', 'Q14C', 'Q14D','Q14E', 'Q16','Q17','Q21A', 
                       'Q21D', 'Q24A', 'Q27C', 'Q30','Q34', 'Q35', 'Q41B')]

write.csv(cordata,'cordata.csv')
##-----did things in excel, sorry, not transparent, not smart but got frusterated----###
cordata2<-read.csv('cordata.csv')
cordata2<-cordata2[,2:30]
library(corrplot)
corr<-cor(cordata2)
rownames(corr)=c("1a)lack openness contributes","1b)poverty/poor edu contr","1c)decline moral contr","1d)casual sex in media contr","1e)inad sex ed contr","3)tv sex should discuss safe","5b)does tv say sex w/o conseq","6)have children still growing up","7)have children <=18","11a)tv kids not allowed - sex","12)timing of birth control/sex info","13a)support sex ed HS","13b)support sex ed MS","13c)support sex ed ES","14a)should HS teach wait marriage","14b)should HS teach condoms","14c)should HS teach basic repro facts","14d)should HS teach when ready","14e)should HS teach how to talk sex w/partner","16)abstinence only or also useful info","17)should HS clinics provide condoms","21a)have had sex talk w/child - repro","21d)have had sex talk w/child - protection","24a)americans uptight about sex","27c)harder to talk sex than have","30)currently in sexual rela.","34)had sex before marriage","35)how many partners","41b)should we talk more about BC")
corrplot(corr,method="square")


