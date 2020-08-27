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
#根據p-value:X對Y表示顯著 R-squared表示模型預測能力
#檢驗殘差獨立性
require(car)
durbinWatsonTest(PERlinearModel)#由於虛無假設H0:殘差間相互獨立，因為p-value > 0.05，代表不會拒絕H0。
#殘差的變異數同質性
ncvTest(PERlinearModel)#由於虛無假設H0:殘差變異數具有同質性，因為p-value > 0.05，代表不會拒絕H0。(這表示上面的線性模型可以使用)
plot(nba_stats$PER, nba_stats$MVP, main = "MVP PER", xlab = "PER", ylab = "MVP or not")
#the mean and median PER of MVPs is much higher than that of NonMVPs .
TSlinearModel <- lm(MVP ~TS., data=nba_stats)
summary(TSlinearModel)

WSlinearModel <- lm(MVP ~ WS, data=nba_stats)
summary(WSlinearModel)

plot(nba_stats$TS., nba_stats$WS, main = "TS% vs WS", xlab = "TS%", ylab = "WS")
plot(nba_stats$TS., nba_stats$PER, main = "TS% vs PER", xlab = "TS%", ylab = "PER")

#跑線性回歸，丟棄最大的p value，直到全部的變數都至少有一星
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
