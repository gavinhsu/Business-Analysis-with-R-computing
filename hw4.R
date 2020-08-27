# Q1

PTF=function(k,a,b,g){
  p.list=PTF.ListInit(a,b,g)
  r.list=c(PTF.r1(a,g))
  for(i in 2:k){
    j.list=c(1:(i-1))
    pn=(b*g*p.list[i-1]+sum(rev(r.list)*p.list*j.list))/i  
    p.list[i]=pn
    
    rn=(i-2+a)/i*g*r.list[i-1]
    r.list[i]=rn 
  }
  tail(p.list,n=1)
}

PTF.ListInit=function(a,b,g){
  p0=0
  if(a==0){
    p0=(1-g)^b
  }
  else{
    p0=exp(b*((1-g)^a-1)/a)
  }
  p1=b*g*p0
  return(c(p1))
}

PTF.r1=function(a,g){
  return((1-a)*g)
}
a1= PTF(9, -3, 2, 0.5) 
a1

#Q2.
#(a)
library(AER)
data(CPS1988)
attach(CPS1988)
experience_square = experience * experience
CPS_lm=lm(log(wage)~experience+experience_square+education+as.factor(ethnicity))
summary(CPS_lm)


#(c)
afam = ifelse(ethnicity=='afam',1,0)
cauc = ifelse(ethnicity=='cauc',1,0)
wage_afam = e^(4.321 + 0.07747*experience - 0.001316*experience_square + 0.08567*education - 0.2434*afam+0.5839)
wage_cauc = e^(4.321 + 0.07747*experience - 0.001316*experience_square + 0.08567*education - 0.2434*cauc+0.5839)

#(d)
glm(log(wage)~experience+experience_square+education+as.factor(ethnicity),family="gaussian")

#(e)
library(quantreg)
cqs_f = log(wage)~ experience + experience_square + education + as.factor(ethnicity)
cqs_rq=rq(cqs_f, tau=seq(0.05, 0.95, 0.05), data=CPS1988)
cqs_rqs=summary(cqs_rq)
plot(cqs_rqs)

#Q3.
#(a)
car.accident=c(rep(0,109),rep(1,65),rep(2,22),rep(3,3),rep(4,1))
car.accident

#(b)
library(MASS)
library(fitdistrplus)
car.accident.fit = fitdist(car.accident,"pois")
summary(car.accident.fit)

#(c)
for (k in c(0:4)){
pois = 200* ((exp(-0.61) * 0.61**k) / factorial(k))
print(paste0("200*P(X=k|£f)=", pois))
}
pois5=1-ppois(4,0.61)
print(paste0("200*P(X >4|£f)=",200*pois5))

#(d)
for (k in c(0:4)){
  bino = 200*dbinom(n, 200, p =0.61/200)
  print(paste0("200*P(X =",k,"|n, p)=",bino))
}
bino5=1-pbinom(4,200,p=0.61/200)
print(paste0("200*P(X >4|n, p)=", bino5))

#Q4.

theta <- seq(0.01,0.49,0.02)
coverage_rateA <- c()
for (i in theta){
  y1 = rbinom(500000,20,i)/20
  coverage_rate <- sum(y1-1.96*sqrt(y1*(1-y1)/20)<i
                      & i<y1+1.96*sqrt(y1*(1-y1)/20))/50000
  coverage_rateA <- append(coverage_rateA,coverage_rate)
}
n<-20
coverage_rateB <- c()
for (i in theta){
  y1 <- rbinom(500000,20,i)/20*(n/(n+4))+(4/(n+4))/2
  coverage_rate <- sum(y1-1.96*sqrt(y1*(1-y1)/20)<i
                      & i<y1+1.96*sqrt(y1*(1-y1)/20))/50000
  coverage_rateB <- append(coverage_rateB,coverage_rate)
}

plot(theta,coverage_rateA,xlab = expression(i),type='l',
     lwd=1,ylim=c(0.65,1),col="Blue")
abline(h=0.95,col="black",lty=1,lwd=1)
lines(theta,coverage_rateB,col="Red",lwd=2)




    

