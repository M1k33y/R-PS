# B1)

R=10
r=3
vol_exact=2*(pi^2)*R*(r^2)

functie_f1=function(x,y,z)
{
  if((sqrt(x^2+y^2)-R)^2+z^2 < (r^2))
    return (1);
  return (0);
}
estimare=function(val,R,r)
{
  a=(2*(R+r))^2*2*r
  
  x=runif(val,min=-r-R,max=R+r)
  y=runif(val,min=-r-R,max=R+r)
  #coord in plan xy
  z=runif(val,min=-r,max=r)
  #coord z
  
  ok=mapply(functie_f1,x,y,z)
  p=mean(ok)
  vol_estimat=a*p
  return (vol_estimat)
}
nr=1
nrval=c(10000,20000,50000)
for(val in nrval)
{
  vol_estimat=estimare(val,R,r)
  err=abs(vol_estimat-vol_exact)/vol_exact
  
  cat("Pentru esantionul",nr,"\n")
  cat("Volum estimat:",vol_estimat,"\n")
  cat("Volum exact:",vol_exact,"\n")
  cat("Eroare relativa:",err,"\n\n")
  
  nr=nr+1
  
}


# B2)

a=0
b=2
c=0
d=3
#am facut pe foaie limitele drept

n=20000
x=runif(n,a,b)
y=runif(n,c,d)

ok= (y>=0) & (y<=2*x) & (y<=6-3*x)
p=mean(ok)

arie_drept=(b-a)*(d-c)
arie_tri=p*arie_drept
cat("Aria triunghiului:",arie_tri)

# B3  a)

f1=function(n)
{
  x=runif(n,-1,1)
  y=(2*x-1)/(x^2-x-6)
  return (mean(y)*2)
}
mc_integ=function(k,n)
{
  val=numeric(k)
  for(i in 1:k)
    val[i]=f1(n)
  medie_est=mean(val)
  sd=sd(val)
  return(list(medie_est,sd))
}
exact=log(3)-log(2)
rez=mc_integ(100,30000)
cat("valoare estimata: ", rez[[1]],"\n" )
cat("deviatia standard: ",rez[[2]],"\n")
cat("valoare exacta: ",exact,"\n")

#B3 b)

f2=function(n)
{
  x=runif(n,3,11)
  y=(x+4)/((x-3)^(1/3))
  return (mean(y)*8)
}
mc_integ=function(k,n)
{
  val=numeric(k)
  for(i in 1:k)
    val[i]=f2(n)
  medie_est=mean(val)
  sd=sd(val)
  return(list(medie_est,sd))
}
exact=61.2
rez=mc_integ(100,30000)
cat("valoare estimata: ", rez[[1]],"\n" )
cat("deviatia standard: ",rez[[2]],"\n")
cat("valoare exacta: ",exact,"\n")

#B3 c)
f3=function(n)
{
  s=0
  for(i in 1:n)
  {
    x=rexp(1)
    s=s+x*exp(-x^2)
  }
  return (s/n)
}
mc_integ=function(k,n)
{
  val=numeric(k)
  for(i in 1:k)
    val[i]=f3(n)
  medie_est=mean(val)
  sd=sd(val)
  return(list(medie_est,sd))
}
exact=1/2
rez=mc_integ(100,30000)
cat("valoare estimata: ", rez[[1]],"\n" )
cat("deviatia standard: ",rez[[2]],"\n")
cat("valoare exacta: ",exact,"\n")

#B4 a)
n=1000
p=0.25
q=0.01
nr_initial=10000
nr_viitor=15000

an=0
nr_curent=nr_initial
while(nr_curent<nr_viitor)
{
  nr_curent=nr_curent+rbinom(1,n,p) -rbinom(1,nr_curent,q)
  
  an=an+1
}
cat("Nr mediu de ani este:",an,"\n")

#B4 b)
set.seed(123)
n=1000
p=0.25
q=0.01
nr_initial=10000
nr_viitor=15000
nr_sim=100
rez=numeric(nr_sim)
for(i in 1:nr_sim)
{
  nr_curent=nr_initial
  for(luni in 1:(40*12+10))
  {
    nr_curent=nr_curent+rbinom(1,n,p) -rbinom(1,nr_curent,q)
  }
  rez[i]=nr_curent
}
prob=sum(rez>=nr_viitor)/nr_sim
cat("Probabilitatea este: ",prob,"\n")

#B4 c)
err <- 0.01
x <- prob
nr_sim <- 100
nr_initial=10000
nr_viitor=15000
while (TRUE) {
  rez <- numeric(nr_sim)
  for (i in 1:nr_sim) {
    nr_curent <- nr_initial
    for (luni in 1:(40 * 12 + 10)) {
      nr_curent <- nr_curent + rbinom(1, n, p) - rbinom(1, nr_curent, q)
    }
    rez[i] <- nr_curent
  }
  prob <- sum(rez >= nr_viitor) / nr_sim
  if (abs(prob - x) <= err) {
    break
  }
  nr_sim <- nr_sim * 2
}
cat("Probabilitatea cu eroare de cel mult 0.01:",prob,"\n")