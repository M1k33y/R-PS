
#A1. a)

Functie1 = function(lambda,p,n,m,k)
{
  valori=k:m
  
  Poisson=dpois(valori,lambda)
  Geometric=dgeom(valori-k,p)
  Binomial=dbinom(valori,n,p)
  
  cat("Pentru valorile:",valori,"\n")
  cat("Distributia Poisson:",Poisson,"\n")
  cat("Distributia Geometrica:",Geometric,"\n")
  cat("Distributia Binomiala:", Binomial,"\n")
}

lambda=3.5
p=0.2
n=10
m=5
k=2
Functie1(lambda,p,n,m,k)

#A1. b)

Functie2 = function(lambda,p,n,m,k)
{
  valori=k:m
  
  Poisson=dpois(valori,lambda)
  Geometric=dgeom(valori-k,p)
  Binomial=dbinom(valori,n,p)

  plot(valori,Poisson,type="l",col="blue",xlab="valori",ylab="probabilitati",main="Functii de masa")
  lines(valori,Geometric,type="l",col="red")
  lines(valori,Binomial,type="l",col="green")
  
  legend("bottomleft",legend=c("Poisson","Geometric","Binomial"),col=c("blue","red","green"),lty=1,lwd=2)
  
}
  
lambda=3.5
p=0.2
n=10
m=5
k=2
Functie2(lambda,p,n,m,k)

#A1. c)

Functie3= function(lambda)
{
  k0=0

  while(ppois(k0,lambda) <= (1-(1/10^6)))
    k0=k0+1
  
  return (k0)
}

lambda=2
k0=Functie3(lambda)
cat("valoarea este: ",k0)

#A2. a)

Functie_a=function(fisier)
{
  
  date=read.csv(fisier)
  
  esantion1=date$P
  esantion2=date$S
  
  
  frec_abs1=as.vector(table(esantion1))
  frec_abs2=as.vector(table(esantion2))
  
  frec_rel1=as.vector(table(esantion1)/length(esantion1))
  frec_rel2=as.vector(table(esantion2)/length(esantion2))
  
  media1=mean(esantion1)
  media2=mean(esantion2)
  
  print("Frecvente absolute esantion1:")
  print(frec_abs1)
  print("Frecvente absolute esantion2:")
  print(frec_abs2)
  print("Frecvente relative esantion1:")
  print(frec_rel1)
  print("Frecvente relative esantion2:")
  print(frec_rel2)
  print("Media esantion1:")
  print(media1)
  print("Media esantion2:")
  print(media2)
  
}
Functie_a("note_PS.csv")

# A2. b)

Functie_b=function(fisier,esantion3)
{
  date=read.csv(fisier,header=TRUE)
  
  esantion=date[[esantion3]]
  
  q1=quantile(esantion,0.25)
  q3=quantile(esantion,0.75)
  iqr=q3-q1
  
  lim_inf=q1-1.5*iqr
  lim_sup=q3+1.5*iqr
  
  newesantion=esantion[esantion>=lim_inf & esantion<=lim_sup]
  hist(newesantion,breaks=seq(1,10,by=1),xlab=esantion3,ylab="Frecventa")
  
}
  Functie_b("note_PS.csv","P")
