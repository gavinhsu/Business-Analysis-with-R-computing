# Q1
# (a)
fib <- function(n) {
  val <- n;
  if (n > 1) val <- fib(n - 1) + fib(n - 2);
  return(val);
}
n <- 1;
while (n > 0 ) {
  #print(fib(n));
  n <- n + 1;
  if(fib(n)>100){
    break
  }
}
print(fib(n))

# (b)
print(n)

# (c)
fib2 = numeric(100)
fib2[1] =1
fib2[2] =1
for (x in 3:100) {
  fib2[x] = fib2[x-1]+fib2[x-2]
}
fib2

# Q2

second.small<-function(x){
  
  if (tryCatch({minimun = min(x[x!=min(x)])},warning = function(msg){return(Inf)}) == Inf){
    return('only one unique number')
  }else {
    return(minimun)
  }
}

second.small(x=c(2, 8, 8, 2, 5, 2, 5, 2))
second.small(x=c(1, 1, 1, 1))

# Q3

f.exist<-function(z,x){
  while(z%in%x == TRUE){
    return(TRUE)
    break()
  }
  return(FALSE)
}

f.exist(z=10, x=c(1:10))
f.exist(z=10, x=c(9, 3, 1))

# Q4

f.divide<-function(z){
  i = 1
  num = 0
  while(z>i){
    if(z%%i==0){
      i = i+1
      num = num+1
    }else{i= i+1}
    
  }
  return(num-1)
}

f.divide(100)

# Q5
f = function(x){
  x**7+10000*x**6+1.06*x**5+10600*x**4+0.0605*x**3+0.0005*x+5
}
f.deriv=function(x){
  7*x**6+60000*x**5+5.3*x**4+42400*x**3+0.1815*x**2+0.0025
}
Newton = function(f,f.deriv,x0,tol){
  x=x0
  while(abs(f(x0))>tol){
    x=x0-(f(x0)/f.deriv(x0))
  }
  return(x)
}
print(f.deriv)

# Q6
BesselI_Gen <- function(a,v,z,max,tolerance){
  item.last = -Inf
  item_in =   item = (1/((gamma((a+1))*factorial(0))**v))*((z/2)**(2*a))
  ans = item_in
  m = 1
  item = (1/((gamma((m+a+1))*factorial(m))**v))*((z/2)**(2*m+a))
  item2 = (1/((gamma((m+a))*factorial(m-1))**v))*((z/2)**(2*(m-1)+a))
  #if(is.na(item)&&is.na(item2)){
  while (m<max && abs(item2-item)>tolerance) {
    ans = ans+item
    m = m+1
  }
  return(ans)
  #}
}
BesselI_Gen(5, 1, 10, 1000, 1e-5)
besselI(10, 5)

