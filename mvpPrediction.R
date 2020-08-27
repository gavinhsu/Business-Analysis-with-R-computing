library(ggplot2)
setwd('C:/Users/GAVIN/Desktop/NBA_stats')
nba_stats <- read.csv('stats_1980.csv', stringsAsFactors = FALSE, header = TRUE)
head(nba_stats)

nba_stats$blanl <- NULL
nba_stats$blank2 <- NULL
str(nba_stats)

sum(is.na(nba_stats))
colSums(is.na(nba_stats))
identical(which(is.na(nba_stats$PER)), which(is.na(nba_stats$ORB.)), which(is.na(nba_stats$DRB.)), which(is.na(nba_stats$TRB.)), which(is.na(nba_stats$USG.)), which(is.na(nba_stats$WS.48)))
which(is.na(nba_stats$PER))
nba_stats[c(12035, 12352, 13315, 14285, 16173), ]
nba_stats<- nba_stats[-c(12035, 12352, 13315, 14285, 16173), ] 
colSums(is.na(nba_stats))

nba_stats <- na.omit(nba_stats)
table(is.na(nba_stats))

PERlinearModel <- lm(MVP ~ PER, data=nba_stats)
summary(PERlinearModel)
#�ھ�p-value:X��Y������� R-squared���ܼҫ��w����O
#����ݮt�W�ߩ�
require(car)
durbinWatsonTest(PERlinearModel)#�ѩ��L���]H0:�ݮt���ۤ��W�ߡA�]��p-value > 0.05�A�N�����|�ڵ�H0�C
#�ݮt���ܲ��ƦP���
ncvTest(PERlinearModel)#�ѩ��L���]H0:�ݮt�ܲ��ƨ㦳�P��ʡA�]��p-value > 0.05�A�N�����|�ڵ�H0�C(�o���ܤW�����u�ʼҫ��i�H�ϥ�)
plot(nba_stats$PER, nba_stats$MVP, main = "MVP PER", xlab = "PER", ylab = "MVP or not")
#the mean and median PER of MVPs is much higher than that of NonMVPs .
TSlinearModel <- lm(MVP ~TS., data=nba_stats)
summary(TSlinearModel)

WSlinearModel <- lm(MVP ~ WS, data=nba_stats)
summary(WSlinearModel)

plot(nba_stats$TS., nba_stats$WS, main = "TS% vs WS", xlab = "TS%", ylab = "WS")
plot(nba_stats$TS., nba_stats$PER, main = "TS% vs PER", xlab = "TS%", ylab = "PER")

#�]�u�ʦ^�k�A���̤j��p value�A����������ܼƳ��ܤ֦��@�P
model <- lm(MVP ~ PER+MP+TS.+ORB.+DRB.+TRB.+(AST/TOV) +USG.+WS+WS.48+eFG., data=nba_stats)
summary(model)
model1 <- lm(MVP ~ PER+MP+TS.+ORB.+DRB.+TRB.+(AST/TOV) +USG.+WS+WS.48, data=nba_stats)
summary(model1)

model2 <- lm(MVP ~ PER+MP+TS.+ORB.+DRB.+TRB.+(AST/TOV)+WS+WS.48, data=nba_stats)
summary(model2)


library(caret)
inTrain <- createDataPartition(y = nba_stats$MVP, p = .60, list = FALSE)
training <- nba_stats[inTrain,]
testing <- nba_stats[-inTrain,]
dim(training)
dim(testing)

MODEL <- glm(MVP ~ G+GS+PER+MP+TS.+FTr+ORB.+DRB.+TRB.+(AST/TOV)+AST.+STL.+BLK.+TOV.+USG.+OWS+DWS+WS+WS.48+OBPM+DBPM+BPM+VORP+FG.+eFG.+FT.+FTA+PF+PTS,
             data=training,family = "binomial")


summary(MODEL)
library(ggfortify)
autoplot(MODEL)