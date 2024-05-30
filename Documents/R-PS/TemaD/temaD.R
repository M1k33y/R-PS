# D1)
zconfidence_interval_fisier = function(fisier, alfa)
{
  data=read.csv(fisier,header=TRUE);
  x=data[[1]]
  xn=mean(x);
  n=length(x);
  sigma=sqrt(92.16);
  critical_z=qnorm((1-alfa/2), 0, 1);
  a=xn-critical_z*sigma/sqrt(n);
  b=xn+critical_z*sigma/sqrt(n);
  interval=c(a, b);
  return(interval);
}

zconfidence_interval_fisier("probabilitati.csv", 0.05);
zconfidence_interval_fisier("probabilitati.csv", 0.01);

# D2)
t_conf_interval <- function(alfa,n,sample_mean,s)
{
  se=s/sqrt(n)
  critical_t=qt(1-alfa/2,n-1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  interval=c(a,b)
  return (interval)
}
t_conf_interval_fisier <- function(fisier, alfa)
{
  x=read.csv(fisier,header=TRUE);
  date=x[[1]]
  sample_mean=mean(date);
  sample_std=sd(date);
  n=length(date);
  interval=t_conf_interval(alfa,n, sample_mean,sample_std);
  return(interval);
}
t_conf_interval_fisier("statistica.csv", 0.05);
t_conf_interval_fisier("statistica.csv", 0.01);

# D3)

test_proportion=function(alfa,p0,n,succese,tip)
{
  p_prim=succese/n
  z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n);
  if(tip=='r')
    critical_z=qnorm(1-alfa);
  if(tip=='l')
    critical_z=qnorm(alfa, 0, 1);
  if(tip=='s')
    critical_z=qnorm(1-alfa/2);
  return (c(z_score,critical_z));
}
p0=0.15
n=100
succese=86
tip='s'

#nivel de seminficatie 1%
alfa=0.01
rez=test_proportion(alfa,p0,n,succese,tip)
cat("Nivel de semnificatie 1%:\n")
cat("z_score:",rez[1],"\n")
cat("critical_z:",rez[2],"\n")

if(abs(rez[1]) > rez[2]){
  cat("Schimbarea a fost utila\n")
} else {
  cat("Schimbarea nu a fost utila\n")
}
#nivel de semnificatie 5%

alfa=0.05
rez=test_proportion(alfa,p0,n,succese,tip)
cat("Nivel de semnificatie 5%:\n")
cat("z_score:",rez[1],"\n")
cat("critical_z:",rez[2],"\n")

if(abs(rez[1]) > rez[2]){
  cat("Schimbarea a fost utila\n")
} else {
  cat("Schimbarea nu a fost utila\n")
}