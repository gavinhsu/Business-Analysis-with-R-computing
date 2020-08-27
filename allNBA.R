library(dplyr) 
library(ggplot2)
library(randomForest)
library(arm) 
library(stringr) 
library(gridExtra) 
library(formattable)
library(corrplot) 

#匯入資料與資料統整`
getwd()
setwd('C:/Users/GAVIN/Desktop/NBA_stats')
seasons <- read.csv("Seasons_Stats.csv", stringsAsFactors = FALSE, header = TRUE)
allNBA <- read.csv("All.NBA.1984-2018.csv", stringsAsFactors = FALSE, header = TRUE)
head(seasons)
head(allNBA)
dim(seasons)
dim(allNBA)

summary(seasons)
summary(allNBA)

#資料處理
seasons <- seasons[ ,-54] 

allNBA$Year <- as.numeric(allNBA$Season)                              
head(allNBA[c(1:10), c(3, 34)], 10) 
postSeasons <- seasons %>% filter(Year > 1998)
postAllNBA<- allNBA %>% filter(Year > 1998 & Year < 2018)

sum(is.na(postSeasons)) 

postSeasons$blanl <- NULL
postSeasons$blank2 <- NULL

colSums(is.na(postSeasons)) 

identical(which(is.na(postSeasons$PER)), which(is.na(postSeasons$USG.)))
which(is.na(postSeasons$PER))
postSeasons[c(3819, 4136, 5099, 6069, 7957), ]
postSeasons<- postSeasons[-c(3819, 4136, 5099, 6069, 7957), ] 

postSeasons[c(9429:9431), ]
postSeasons <- subset(postSeasons, !Tm == "TOT")

which(postAllNBA$Tm == "TOT")
postAllNBA[239, 5] <- "ATL"
postAllNBA[180, 5] <- "DEN"

#new dataframe 
perGame <- postSeasons %>% mutate(Name = Player, Position = Pos, age = Age, year = Year,  Team = Tm, Games = G, Starts = GS, Minutes = MP/G, Points = PTS/G, Rebounds = TRB/G, Assists = AST/G, Steals = STL/G, Blocks = BLK/G, Turnovers = TOV/G, Fouls = PF/G, FTs = FT/G, Threes = X3P/G, FGs = FG/G, Usage = USG., EfficiencyRating = PER, BoxPlusMinus = BPM, ShootingPercentage = eFG.)
perGame <- perGame[ , c(53:74)]
str(perGame)
two.digit.round <- function(x) round(x, 2)
perGame[ , c(8:18)] <- sapply(perGame[ , c(8:18)], two.digit.round)
str(perGame)

perGame <- perGame %>% filter(Games > 10 & Minutes > 5)

perGame$ID <- str_c(substr(perGame$Name, start = 1, stop = 3), substr(perGame$age, start = 1, stop = 2), substr(perGame$Team, start = 1, stop = 3), substr(perGame$year, start = 3, stop = 4), sep = "")
postAllNBA$ID <- str_c(substr(postAllNBA$Player, start = 1, stop = 3), substr(postAllNBA$Age, start = 1, stop = 2), substr(postAllNBA$Tm, start = 1, stop = 3), substr(postAllNBA$Year, start = 3, stop = 4), sep = "")
perGame$All.NBA <- ifelse(perGame$ID %in% postAllNBA$ID, 1, 0)
sum(perGame$All.NBA) 

nba.pergame.check <- perGame %>% filter(All.NBA == 1) %>% group_by(year) %>% summarise(length(Name))
nba.pergame.check


perGame[perGame$year == 2013 & perGame$All.NBA == 1, ]
perGame[6047, 24] <- 0

#資料視覺化
#各項數據密度圖
pointsDensity <- ggplot(perGame, aes(Points)) + geom_density(fill = "skyblue") + geom_vline(aes(xintercept = mean(Points)), linetype = "dashed")
reboundsDensity <- ggplot(perGame, aes(Rebounds)) + geom_density(fill = "#E69F00") + geom_vline(aes(xintercept = mean(Rebounds)), linetype = "dashed")
assistsDensity <- ggplot(perGame, aes(Assists)) + geom_density(fill = "#FF6666") + geom_vline(aes(xintercept = mean(Assists)), linetype = "dashed")
turnoversDensity <- ggplot(perGame, aes(Turnovers)) + geom_density(fill = "#999999") + geom_vline(aes(xintercept = mean(Turnovers)), linetype = "dashed")
grid.arrange(pointsDensity, reboundsDensity, assistsDensity, turnoversDensity, ncol = 2)

#以年齡層作為區間球員數對應的效率巔峰
efficientAge <- perGame %>% group_by(age) %>% summarise(Efficiency = mean(EfficiencyRating), Players = length(Name))
ggplot(efficientAge, aes(age, Efficiency)) + 
  geom_point(aes(size = Players), colour = "skyblue") + 
  geom_smooth(method = "loess", colour = "seashell4", se = FALSE, linetype = "dashed") + 
  theme_bw()

#和efficiencyrating相關的變數
usagePER <- ggplot(perGame, aes(Usage, EfficiencyRating)) + geom_point(colour = "#E69F00", alpha = 0.5) + geom_smooth(method = lm, colour = "#999999", linetype = "dashed")
minutesPER <- ggplot(perGame, aes(Minutes, EfficiencyRating)) + geom_point(colour = "#E69F00", alpha = 0.5) + geom_smooth(method = lm, colour = "#999999", linetype = "dashed")
threesPER <- ggplot(perGame, aes(Threes, EfficiencyRating)) + geom_point(colour = "#E69F00", alpha = 0.5) + geom_smooth(method = lm, colour = "#999999", linetype = "dashed")
shootingPER <- ggplot(perGame, aes(ShootingPercentage, EfficiencyRating)) + geom_point(colour = "#E69F00", alpha = 0.5) + geom_smooth(method = lm, colour = "#999999", linetype = "dashed")
grid.arrange(minutesPER, threesPER, usagePER,shootingPER)
#ShootingPercentage在50%特別突出
#三分球不適合用線性表示

#各變數相關性
matrix <- as.matrix(perGame[ , c(6:20)])
corrplot(cor(matrix), is.corr = FALSE, method = "circle", type = "upper")

perGame$All.NBA <- as.factor(perGame$All.NBA) 

logPoints <- glm(All.NBA ~ Points, family = binomial, data = perGame) 
pointsProbability <- data.frame(Points = seq(0, 40, 0.1))
pointsPrediction <- predict(logPoints, pointsProbability, type = "response") 
pointsProbability <- cbind(pointsProbability, pointsPrediction)
names(pointsProbability) <- c("Points", "Probability") 

pointsGraph <- ggplot(pointsProbability, aes(Points, Probability)) + geom_line() + 
  geom_vline(xintercept = mean(perGame$Points), colour = "deepskyblue", linetype = "dashed")+ 
  geom_vline(xintercept = quantile(perGame$Points, 0.99), colour = "sienna2", linetype = "dashed") + 
  annotate("text", x = 24, y = 0.8, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + 
  annotate("text", x = 7, y = 0.8, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3)

## rebounds
logRebounds <- glm(All.NBA ~ Rebounds, family = binomial, data = perGame)
reboundsProbability <- data.frame(Rebounds = seq(0, 25, 0.1))
reboundsPrediction <- predict(logRebounds, reboundsProbability, type = "response") 
reboundsProbability <- cbind(reboundsProbability, reboundsPrediction)
names(reboundsProbability) <- c("Rebounds", "Probability") 
reboundsGraph <- ggplot(reboundsProbability, aes(Rebounds, Probability)) + geom_line() + 
  geom_vline(xintercept = mean(perGame$Rebounds), colour = "deepskyblue", linetype = "dashed") + 
  geom_vline(xintercept = quantile(perGame$Rebounds, 0.99), colour = "sienna2", linetype = "dashed") + 
  annotate("text", x = 10.5, y = 0.75, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) +
  annotate("text", x = 3, y = 0.75, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## assists
logAssists <- glm(All.NBA ~ Assists, family = binomial, data = perGame)
assistsProbability <- data.frame(Assists = seq(0, 20, 0.1))
assistsPrediction <- predict(logAssists, assistsProbability, type = "response") 
assistsProbability <- cbind(assistsProbability, assistsPrediction)
names(assistsProbability) <- c("Assists", "Probability") 
assistsGraph <- ggplot(assistsProbability, aes(Assists, Probability)) + geom_line() + 
  geom_vline(xintercept = mean(perGame$Assists), colour = "deepskyblue", linetype = "dashed") + 
  geom_vline(xintercept = quantile(perGame$Assists, 0.99), colour = "sienna2", linetype = "dashed") + 
  annotate("text", x = 8, y = 0.75, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + 
  annotate("text", x = 1.2, y = 0.75, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## PER
logPER <- glm(All.NBA ~ EfficiencyRating, family = binomial, data = perGame)
PERProbability <- data.frame(EfficiencyRating = seq(0, 40, 0.1))
PERPrediction <- predict(logPER, PERProbability, type = "response")
PERProbability <- cbind(PERProbability, PERPrediction)
names(PERProbability) <- c("PER", "Probability")
PERGraph <- ggplot(PERProbability, aes(PER, Probability)) + geom_line() + 
  geom_vline(xintercept = mean(perGame$EfficiencyRating), colour = "deepskyblue", linetype = "dashed") + 
  geom_vline(xintercept = quantile(perGame$EfficiencyRating, 0.99), colour = "sienna2", linetype = "dashed") + 
  annotate("text", x = 24, y = 0.9, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + 
  annotate("text", x = 11, y = 0.9, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## print graphs
grid.arrange(pointsGraph, reboundsGraph, assistsGraph, PERGraph, top = "被選進最佳陣容的機率對比能力值")






