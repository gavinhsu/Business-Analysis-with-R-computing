#Q1.
#(a) 
seq(1,100,2)
odd = c(seq(1,100,2))

#(b)
mean(odd)
median(odd)
min(odd)
max(odd)
range(odd)

#(c)
sqrt(var(odd))
quantile(odd,0.05)
quantile(odd,0.95)

#(d)
seq(2,100,2)
even <- c(seq(2,100,2))

#(e)
mean(odd) > mean(even)
var(odd) > var(even)

#Q2.
rep(c(seq(0,4)),each=5)
rep(c(seq(1,5)),5)

#Q3.
#(a)
seq(1,100)
seq <- seq(1,100)
which(seq%%2!=0&seq%%3!=0&seq%%7!=0)

#(b)
a<- diag(10)
a
a[a==1] <-5 #first one
b
a[which(a==1)]<-5 #second one
c

#Q4.
#(a)
factorial(4)
factorial(50)
factorial(5000)

#(b)
choose(4,2)
choose(50,20)
choose(5000,2000)

#(c)
sum <- 0
for (i in 1:5000){
  sum <- sum +log(i)
}
sum

#Q5.
#(a)
f.exp <- expression(x^4+2*x^2+e^x)
deriv(f.exp,namevec = 'x')
f.exp <- expression(4*x^3+2*(2*x)+e^x)
deriv(f.exp,namevec = 'x')

#(b)
e <- exp(1)
f1 = expression(x^4+2*x^2+e^x)
g1 = D(f1,"x")
point1 = function(x){eval(g1)}
ans1 <- point1(3)
ans1

f2 = expression(4*x^3+2*(2*x)+e^x)
g2 = D(f2,"x")
point2 = function(x){eval(g2)}
ans2 <- point2(3)
ans2

#Q6.
heart_up = function(x) {sqrt(1-(abs(x)-1)^2)}
heart_lo = function(x) {acos(1-abs(x))-pi}
x=seq(-2, 2, 0.05)
plot(x, heart_lo(x), ylim=c(heart_lo(0),1), type="l")
lines(x, heart_up(x))

show_heart <- function(x){
  heart_up = function(x){sqrt(1-(abs(x)-1)^2)}
  heart_lo = function(x){acos(1-abs(x))-pi}
  plot(x,heart_lo(x),ylim=c(heart_lo(0),1),type="l")
  lines(x,heart_up(x))
}
show_heart
integrate(show_heart,-2,2)
#Q7.
data("Orange")
#(a)
ora <- Orange
plot(x=ora$circumference,y=log(ora$age),
     ylim = c(4, 8))
cor(x=ora$circumference,y=log(ora$age))
#x and y linear

#(b)
errorI <- function(w1,w2) {
  
  for (i in ora) {
    
    error <-((log(ora$age))-((w1*ora$circumference)/(w2+ora$circumference)))**2
  }
  
  return(sum(error))
}
(errorI(0.5,0.5))

#(c)
optim(c(0.5,0.5),errorI)


