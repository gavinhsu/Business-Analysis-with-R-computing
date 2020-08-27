# 1.
# (a)
# p(A and C) = 1/6  
# p(B or C)= 1/2 
# p(A and (B or C)) = 4/9
# (b)
dice1 <- sample(1:6, size = 1000, replace = TRUE)
dice2 <- sample(1:6, size = 1000, replace = TRUE)

pAandC <- function() {
  num = 0 #發生事件
  i = 1 #投擲次數
  while (i < 1001) {
    if (dice1[i] == 4 && dice1[i] + dice2[i] >= 5) {
      num = num + 1
    }
    i = i + 1
  }
  p <- num / 1000
  return(p)
}
pBorC <- function() {
  num = 0 #發生事件
  i = 1 #投擲次數
  while (i < 1001) {
    if (dice1[i]>dice2[i] || dice1[i] ==4) {
      num = num + 1
    }
    i = i + 1
  }
  p <- num / 1000
  return(p)
}
pAandBorC <- function() {
  num = 0 #發生事件
  i = 1 #投擲次數
  while (i < 1001) {
    if (dice1[i] + dice2[i] >= 5 &&(dice1[i]>dice2[i] || dice1[i] ==4)) {
      num = num + 1
    }
    i = i + 1
  }
  p <- num / 1000
  return(p)
}
pAandC()
pBorC()
pAandBorC()

# 2.
# (a)
# P(both black or both blue or both green) = 7/24*6/23 + 8/24*7/23 + 9/24*8/23
#                                          = (42+56+72)/552 = 170/552 = 85/276
#P(black) = 7/24*6/23 = 42/552 = 7/92

# (b)
socks <- rep(c(1,3,5), c(7,8,9))
result <- replicate(5000, sample(socks, 2, replace=FALSE))
point <- result[1,] + result[2,]
match <- sum(point == 2) + sum(point == 6) + sum(point == 10)
both_black <- sum(point == 2)
prob_match <- match/5000
prob_black_pair <- both_black/5000
print(paste(c("simulated probability that they match: ", prob_match), collapse=" "))
print(paste(c("simulated probability that a black pair is chosen: ", prob_black_pair), collapse=" "))

# 3.
#(a)
#choose one person from every ethnic group first and then add one more person randomly to the mix.
#P(all four groups are chosen) = 5/12*2/11*3/10*2/9=60/11880=1/198

#(b)
people <- rep(c(0,10,100,1000), c(5,2,3,2))
result <- replicate(5000, sample(people, 5, replace=FALSE))
point <- result[1,] + result[2,] + result[3,] + result[4,] + result[5,]
match <- sum(point == 1110) + sum(point == 1120) + sum(point == 1210) + sum(point == 2110)
prob_all_chosen <- match/5000
print(paste(c("simulated probability that people from all ethnics groups are chosen: ", prob_all_chosen), collapse=" "))
# It isn't close to the probability in part (a)

# 4.
# (a)
a<-(choose(4,1)*choose(16,1))/choose(52,2)
a
# (b)
cards = c(1:52)
results = replicate(50000,sample(cards,2,replace = FALSE))
success = 0
for (i in c(1:50000)) {
  
  if (results[1,i]<=4 && results[1,i]>=1 && results[2,i]<=52 && results[2,i]>=37){
    success = success+1
  }
  else if (results[2,i]<=4 && results[2,i]>=1 && results[1,i]<=52 && results[1,i]>=37){
    success = success+1
  }
  Psuccess = success/50000
}
Psuccess
# very close to classic one from (a) if it runs 50000 times
results = replicate(50,sample(cards,2,replace = FALSE))
success2 = 0
for (i in c(1:50)) {
  
  if (results[1,i]<=4 && results[1,i]>=1 && results[2,i]<=52 && results[2,i]>=37){
    success2 = success2+1
  }
  else if (results[2,i]<=4 && results[2,i]>=1 && results[1,i]<=52 && results[1,i]>=37){
    success2 = success2+1
  }
  Psuccess2 = success2/50
}
Psuccess2
# not close to classic one from (a) at all if it only runs 50 times

# 5.
# (a)
a <- log(choose(10000,100))+log(factorial(100))-log(10)*400

b <- exp(a)
prob <- 1-b # 1 - 完全不重複的機率
prob
# (b)
IDs = c(0:9999)
choice <- replicate(5000,sample(IDs,100,replace = TRUE))
num = 0
for(i in c(1:5000)){
  column<-choice[,i]
  test<-duplicated(column)
  for (k in(1:100)){
    if(test[k] == TRUE){
      num = num+1
      break
    }
    
  }
}
prob2 = num/5000
prob2

# (c)
for(min in c(1:1000)){
  a <- log(choose(10000,min))+log(factorial(min))-log(10000)*min
  b <- exp(a)
  prob <- 1-b 
  prob
  if (prob>=0.5){
    return(min)
    break
  }
}
min

