set.seed(123)

# C1 a)
perm <- function(n) {
  u <- runif(n)
  permutare <- order(u)
  return(permutare)
}

#C1 b)
matrice <- function(n, k) {
  cuvinte <- matrix(sample(c(0, 1), n * k, replace = TRUE), nrow = n, ncol = k)
  return(cuvinte)
}

bit_gen <- function() {
  return(sample(c(0, 1), 1))
}

compara <- function(Wi, Wj) {
  lg <- min(length(Wi), length(Wj))
  
  for (i in 1:lg) {
    if (Wi[i] < Wj[i]) {
      return(TRUE)
    } else if (Wi[i] > Wj[i]) {
      return(FALSE)
    }
  }
  
  while (TRUE) {
    bit1 <- bit_gen()
    bit2 <- bit_gen()
    
    if (bit1 < bit2) {
      return(TRUE)
    } else if (bit1 > bit2) {
      return(FALSE)
    }
  }
}

n <- 10
k <- 4
perm1 <- perm(n)
cat("Perm random:", n, ":\n")
print(perm1)

matrice1 <- matrice(n, k)
cat("Matrice binara:\n")
print(matrice1)

matrice2 <- matrice1[perm1, ]
cat("Matrice permutata:\n")
print(matrice2)

Wi <- matrice2[1, ]
Wj <- matrice2[2, ]
rez <- compara(Wi, Wj)
cat("Este Wi mai mic decat Wj?:", rez, "\n")

quick_sort <- function(cuvinte) {
  if (length(cuvinte) <= 1)
    return(cuvinte)
  
  index <- sample(1:length(cuvinte), 1)
  pivot <- cuvinte[[index]]
  
  nr1 <- list()  # mic
  nr2 <- list()  # egal
  nr3 <- list()  # mare
  
  for (i in seq_along(cuvinte)) {
    if (identical(cuvinte[[i]], pivot)) {
      nr2 <- c(nr2, list(cuvinte[[i]]))
    } else if (compara(cuvinte[[i]], pivot)) {
      nr1 <- c(nr1, list(cuvinte[[i]]))
    } else {
      nr3 <- c(nr3, list(cuvinte[[i]]))
    }
  }
  
  sort_nr1 <- quick_sort(nr1)
  sort_nr3 <- quick_sort(nr3)
  
  return(c(sort_nr1, nr2, sort_nr3))
}

#C1 d)

n <- 7
k <- 4

perm1 <- perm(n)
cat("Random perm:", n, ":\n")
print(perm1)

matrice1 <- matrice(n, k)
cat("Siruri random:\n")
print(matrice1)

cuvinte <- as.list(1:n)
for (i in seq_along(cuvinte)) {
  cuvinte[[i]] <- matrice1[i, ]
}

sorted_words <- quick_sort(cuvinte)
cat("Siruri sortate:\n")
print(sorted_words)

# C2 

max_cut=function(n)
{
  graph=matrix(0,nrow=n*2,ncol=n*2)
  
  #adaug muchii intre cele 2 jum de noduri
  graph[1:n,(n+1):(2*n)]=1
  graph[(n+1):(2*n),1:n]=1
  
  #aleg n noduri pentru A si restul la B
  A=sample(1:(n*2),n)
  B=setdiff(1:(n*2),A)
  
  cut_edges=graph[A,B]
  nr=sum(cut_edges)
  cat("multime A: ",A, "\n")
  cat("multime B",B,"\n")
  cat("taieturi:\n")
  print(cut_edges)
  cat("nr taieturi: ",nr)
}
n=5
max_cut(n)

# C2 b)
max_cut2=function(n)
{
  graph=matrix(0,nrow=n*2,ncol=n*2)
  
  #adaug muchii intre cele 2 jum de noduri
  graph[1:n,(n+1):(2*n)]=1
  graph[(n+1):(2*n),1:n]=1
  
  #aleg n noduri pentru A si restul la B
  A=sample(1:(n*2),n)
  B=setdiff(1:(n*2),A)
  
  cut_edges=graph[A,B]
  nr=sum(cut_edges)
  return (list(A=A,B=B,cut_edges=cut_edges,nr=nr))
}

sanse_crescute=function(n,nr_incercari)
{
  nr_max=0
  nr_cut=0
  
  for(i in 1:nr_incercari)
  {
    rez=max_cut2(n)
    if(rez$nr>nr_max){
      nr_cut=rez
      nr_max=rez$nr
    }
  }
  return (nr_cut)
}

n=5
result=sanse_crescute(n,5000)
print(result)