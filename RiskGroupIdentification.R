library(ggplot2)
library(corrplot)

load('PRAMdata.rda')

unintended <- droplevels(rd[rd$QuestionId == 'QUO265',])

rel.cols <- c('Response', 'Sample_Size', 'Break_Out')

temp <- droplevels(unintended[unintended$Break_Out_Category == 'Marital Status',])
temp <- droplevels(temp[temp$Response != '', rel.cols])
temp <- na.omit(temp)
scaled <- data.frame(Break_Out = factor(), Response = factor(), total = integer())
for(bo in levels(temp$Break_Out)){
  t <- temp[temp$Break_Out == bo, ]
  tot_answers <- sum(t$Sample_Size)
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'Yes',
                                     prop = sum(t[t$Response == 'YES', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'YES', 'Sample_Size'])))
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'No',
                                     prop = sum(t[t$Response == 'NO', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'NO', 'Sample_Size'])))
  
}


ggplot(scaled, aes(x=Response, y= prop, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('skyblue', 'skyblue4','pink2', 'pink3', 'pink4')) + 
  ylab('Proportion') +
  ggtitle('Proportion of Unplanned Pregnancies by Marital Status') +
  guides(fill=guide_legend(title="Marital Status"))


ggplot(scaled, aes(x=Response, y= total, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('skyblue', 'skyblue4','pink2', 'pink3', 'pink4')) + 
  ylab('Total Number of Responses') +
  ggtitle('Total Number of Unplanned Pregnancies by Marital Status') +
  guides(fill=guide_legend(title="Marital Status"))




temp <- droplevels(unintended[unintended$Break_Out_Category == 'Income (years 2004 and beyond)',])
temp <- droplevels(temp[temp$Response != '', rel.cols])
temp <- na.omit(temp)
scaled <- data.frame(Break_Out = factor(), Response = factor(), total = integer())
corr <- scaled
corr$med_inc <- c(12500,12500, 37500,37500, 99000,99000,5000, 5000)
cor(corr[corr$Response == 'Yes', 'med_inc'],corr[corr$Response == 'Yes', 'total'])
cor(corr[corr$Response == 'Yes', 'med_inc'],corr[corr$Response == 'Yes', 'prop'])
cor(corr[corr$Response == 'No', 'med_inc'],corr[corr$Response == 'No', 'total'])
cor(corr[corr$Response == 'No', 'med_inc'],corr[corr$Response == 'No', 'prop'])


for(bo in levels(temp$Break_Out)){
  t <- temp[temp$Break_Out == bo, ]
  tot_answers <- sum(t$Sample_Size)
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'Yes',
                                     prop = sum(t[t$Response == 'YES', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'YES', 'Sample_Size'])))
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'No',
                                     prop = sum(t[t$Response == 'NO', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'NO', 'Sample_Size'])))
  
}

order <- c("Less than $10,000", 
           "$10,000 to $24,999", 
           "$25,000 to $49,999", 
           "$50,000 or more")
scaled$Break_Out <- factor(scaled$Break_Out, order)

ggplot(scaled, aes(x=Response, y= prop, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('pink', 'pink1','pink2', 'pink3', 'pink4')) + 
  ylab('Proportion') +
  ggtitle('Proportion of Unplanned Pregnancies by Income') +
  guides(fill=guide_legend(title="Income Bracket"))


ggplot(scaled, aes(x=Response, y= total, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('pink', 'pink1','pink2', 'pink3', 'pink4')) + 
  ylab('Total Number of Responses') +
  ggtitle('Total Number of Unplanned Pregnancies by Income') +
  guides(fill=guide_legend(title="Income Bracket"))




temp <- droplevels(unintended[unintended$Break_Out_Category == 'Maternal Age - 18 to 44 years in groupings',])
temp <- droplevels(temp[temp$Response != '', rel.cols])
temp <- na.omit(temp)
scaled <- data.frame(Break_Out = factor(), Response = factor(), total = integer())
for(bo in levels(temp$Break_Out)){
  t <- temp[temp$Break_Out == bo, ]
  tot_answers <- sum(t$Sample_Size)
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'Yes',
                                     prop = sum(t[t$Response == 'YES', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'YES', 'Sample_Size'])))
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'No',
                                     prop = sum(t[t$Response == 'NO', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'NO', 'Sample_Size'])))
  
}


ggplot(scaled, aes(x=Response, y= prop, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('pink', 'pink1','pink2', 'pink3', 'pink4')) + 
  ylab('Proportion') +
  ggtitle('Proportion of Unplanned Pregnancies by Age') +
  guides(fill=guide_legend(title="Age Category"))


ggplot(scaled, aes(x=Response, y= total, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('pink', 'pink1','pink2', 'pink3', 'pink4')) + 
  ylab('Total Number of Responses') +
  ggtitle('Total Number of Unplanned Pregnancies by Age') +
  guides(fill=guide_legend(title="Age Category"))




temp <- droplevels(unintended[unintended$Break_Out_Category == 'Maternal Education',])
temp <- droplevels(temp[temp$Response != '', rel.cols])
temp <- na.omit(temp)
scaled <- data.frame(Break_Out = factor(), Response = factor(), total = integer())
for(bo in levels(temp$Break_Out)){
  t <- temp[temp$Break_Out == bo, ]
  tot_answers <- sum(t$Sample_Size)
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'Yes',
                                     prop = sum(t[t$Response == 'YES', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'YES', 'Sample_Size'])))
  scaled <- rbind(scaled, data.frame(Break_Out = bo, Response = 'No',
                                     prop = sum(t[t$Response == 'NO', 'Sample_Size'])/tot_answers,
                                     total = sum(t[t$Response == 'NO', 'Sample_Size'])))
  
}
levels(scaled$Break_Out) <- c('Less than 12 years', 'More than 12 years', '12 years')
order <- c('Less than 12 years', '12 years', 'More than 12 years')
scaled$Break_Out <- factor(scaled$Break_Out, order)

corr <- scaled
corr$edu <- c(8,8, 16,16, 12,12)
cor(corr[corr$Response == 'Yes', 'edu'],corr[corr$Response == 'Yes', 'total'])
cor(corr[corr$Response == 'Yes', 'edu'],corr[corr$Response == 'Yes', 'prop'])
cor(corr[corr$Response == 'No', 'edu'],corr[corr$Response == 'No', 'total'])
cor(corr[corr$Response == 'No', 'edu'],corr[corr$Response == 'No', 'prop'])

ggplot(scaled, aes(x=Response, y= prop, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('pink', 'pink2', 'pink3', 'pink4')) + 
  ylab('Proportion') +
  ggtitle('Proportion of Unplanned Pregnancies by Education') +
  guides(fill=guide_legend(title="Years of Education"))


ggplot(scaled, aes(x=Response, y= total, fill=Break_Out)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values=c('pink', 'pink2', 'pink3', 'pink4')) + 
  ylab('Total Number of Responses') +
  ggtitle('Total Number of Unplanned Pregnancies by Education') +
  guides(fill=guide_legend(title="Years of Education"))

