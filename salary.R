library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)

salary.table <- 
  read.csv("/Users/wukuanwei/Desktop/R期末/NBA_season1718_salary.csv")
ss <- read.csv("/Users/wukuanwei/Desktop/R期末/Seasons_Stats.csv")
str(salary.table)
str(ss)

stats17 <- 
  ss %>% filter(Year >= 2017) %>% 
  select(Year:G, MP:TS.,WS,WS.48, FG:PTS) %>% 
  distinct(Player, .keep_all = TRUE) %>% 
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G,
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G) #新增變量（各數據場均）

stats_salary <- merge(stats17, salary.table, by.x = "Player", by.y = "Player")
names(stats_salary)[43] <- "salary17_18"
stats_salary <- stats_salary[-42]
corrplot(cor(stats_salary %>% 
               select(salary17_18, MPG:SPG, 
                      Age, PER,TS.,WS.48, contains("%")), 
             use = "complete.obs"), 
         method = "circle",type = "upper")#檢查共線性

stats_salary_Guard<-stats_salary%>%filter(Pos%in%c("PG","SG"))
stats_salary_Forward<-stats_salary%>%filter(Pos%in%c("SF","PF"))
stats_salary_Center<-stats_salary%>%filter(Pos%in%c("C"))
corrplot(cor(stats_salary_Guard %>% 
               select(salary17_18, MPG:SPG, 
                      Age, PER,TS.,WS.48, contains("%")), 
             use = "complete.obs"), 
         method = "circle",type = "upper")#檢查共線性


stats_salary_cor <- 
  stats_salary %>% 
  select(salary17_18, PPG, MPG, RPG, PER, SPG,BPG, APG,WS.48,TOPG)
ggpairs(stats_salary_cor)

stats_salary_cor_G <- 
  stats_salary_Guard %>% 
  select(salary17_18, PPG, MPG, RPG, PER, SPG,BPG, APG,WS.48,TOPG)

cor(stats_salary_cor_G)[,"salary17_18"]

stats_salary_cor_F <- 
  stats_salary_Forward %>% 
  select(salary17_18, PPG, MPG, RPG, PER, SPG,BPG, APG,WS.48,TOPG)

cor(stats_salary_cor_F)[,"salary17_18"]

stats_salary_cor_C <- 
  stats_salary_Center %>% 
  select(salary17_18, PPG, MPG, RPG, PER, SPG,BPG, APG,WS.48,TOPG)

cor(stats_salary_cor_C)[,"salary17_18"]
names(stats_salary)[5] <- "Team"
plot_ly(data = stats_salary, x = ~salary17_18, y = ~PPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>PPG: ", round(PPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs Point Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Point per Game")
  )
plot_ly(data = stats_salary, x = ~salary17_18, y = ~APG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>PPG: ", round(APG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs assist Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Assist per Game")
  )
plot_ly(data = stats_salary, x = ~salary17_18, y = ~RPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>PPG: ", round(RPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
                        title = "Salary vs rebound Per Game",
                        xaxis = list(title = "Salary USD"),
                        yaxis = list(title = "rebound per Game")
                      )

stats_salary %>% 
  ggplot(aes(x = salary17_18, y = PPG)) + 
  geom_point() + 
  geom_smooth(method = "lm") 
stats_salary %>% 
  ggplot(aes(x = salary17_18, y = APG)) + 
  geom_point() + 
  geom_smooth(method = "lm") 
stats_salary %>% 
  ggplot(aes(x = salary17_18, y = RPG)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

stats_salary_regression <- 
  stats_salary %>% select(salary17_18, MPG:SPG)
summary(lm(salary17_18~., data=stats_salary_regression))


avg.minutes <- mean(stats_salary_regression$MPG)
avg.turnover <- mean(stats_salary_regression$TOPG)
avg.assist<- mean(stats_salary_regression$APG)
avg.block<- mean(stats_salary_regression$BPG)
avg.steal<- mean(stats_salary_regression$SPG)
avg.block
stats_salary_regression$Trusted <- as.factor(ifelse(stats_salary_regression$MPG >= avg.minutes, "Yes", "No"))
stats_salary_regression$Agressiveness <- as.factor(ifelse(stats_salary_regression$TOPG >= avg.turnover, "Yes", "No"))
stats_salary_regression$BigBrain <- as.factor(ifelse(stats_salary_regression$APG >= avg.assist, "Yes", "No"))
stats_salary_regression$Blocker <- as.factor(ifelse(stats_salary_regression$BPG >= avg.block, "Yes", "No"))
stats_salary_regression$Stealer <- as.factor(ifelse(stats_salary_regression$SPG >= avg.steal, "Yes", "No"))

head(stats_salary_regression)

stats_salary_regression %>% 
  ggplot(aes(x = salary17_18, y = APG, colour = BigBrain)) + 
  geom_point() + 
  geom_smooth(method="lm")
stats_salary_regression %>% 
  ggplot(aes(x = salary17_18, y = TOPG, colour = Agressiveness)) + 
  geom_point() + 
  geom_smooth(method="lm")
stats_salary_regression %>% 
  ggplot(aes(x = salary17_18, y = MPG, colour = Trusted)) + 
  geom_point() + 
  geom_smooth(method="lm")
stats_salary_regression %>% 
  ggplot(aes(x = salary17_18, y = BPG, colour = Blocker)) + 
  geom_point() + 
  geom_smooth(method="lm")
stats_salary_regression %>% 
  ggplot(aes(x = salary17_18, y = SPG, colour = Stealer)) + 
  geom_point() + 
  geom_smooth(method="lm")

salary_prediction <- function(m, point, minutes,assist,rebound,turnover,steal,block){
  pre_new <- predict(m, data.frame(PPG = point, MPG = minutes,APG = assist,RPG= rebound,TOPG = turnover,SPG = steal,BPG = block))
  msg <- paste("PPG:", point, ",MPG:", minutes, ",APG:",assist,",RPG:",rebound,",TOPG:",turnover, 
               ",SPG:",steal, ",BPG:",block,
               " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}
model <- lm(formula = salary17_18 ~ PPG + MPG+ APG + RPG +TOPG +SPG+BPG , data = stats_salary_regression)
summary(model)
salary_prediction(model, 25.2, 35.1,5.8,3.2,2.5,1.2,0.3)
