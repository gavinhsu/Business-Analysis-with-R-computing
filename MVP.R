install.packages("ggfortify")
install.packages('Rcpp')

library(ggfortify)
library(ROCR)
nba_stats <- read.csv('./Documents/uni/r/hw/data/stats_1980_2.csv')
head(nba_stats)

nba_stats$blanl <-NULL
nba_stats$blank2 <-NULL
str(nba_stats)

sum(is.na(nba_stats))
colSums(is.na(nba_stats))
identical(which(is.na(nba_stats$PER)), which(is.na(nba_stats$ORB.)), which(is.na(nba_stats$DRB.)), which(is.na(nba_stats$TRB.)), which(is.na(nba_stats$USG.)), which(is.na(nba_stats$WS.48)))
which(is.na(nba_stats$PER))
nba_stats[c(12035, 12352, 13315, 14285, 16173), ]
nba_stats<- nba_stats[-c(12035, 12352, 13315, 14285, 16173), ] 
colSums(is.na(nba_stats))

nba_stats<- na.omit(nba_stats)
table(is.na(nba_stats))

set.seed(555) 
train.index = sample(1:14585, 14585*0.7, replace=FALSE)  
training = nba_stats[train.index,]
testing = nba_stats[-train.index,]

model <- glm(MVP ~ G+GS+PER+MP+TS.+FTr+ORB.+DRB.+TRB.+(AST/TOV)+AST.+STL.+BLK.+TOV.+USG.+OWS+DWS+WS+WS.48+OBPM+DBPM+BPM+VORP+FG.+eFG.+FT.+FTA+PF+PTS, data=nba_stats,family = "binomial")
model <- glm(formula= MVP ~ GS+TS.+STL.+BLK.+TOV.+USG.+WS.48+OBPM+eFG.+SPG+BPG, data=nba_stats,family = "binomial")
nba_stats$MVPpredict<-0
nba_stats$MVPpredict<-model$fitted.values
summary(model)
model

write.csv(nba_stats, file='./Documents/uni/r/hw/data/nba_predictions.csv')


