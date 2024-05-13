#punct b
f= function(x)exp(x)
val1=integrate(f,1,4)$value

x=seq(1,4,length.out=10001)
y=f(x)

val2=(4-1)/20000*(sum(y)-y[1]-y[10000+1]+2*sum(y[2:10000]))

abs_err=abs(val1-val2)
rel_err=abs_err/val1*100
print(val1)
print(val2)
print(abs_err)
print(rel_err)

#punct d
f1 <- function(N) {
  sum = 0
  for (i in 1:N) {
    x = rexp(1,1)
    sum = sum + (1 / (4 * x * x - 1))/exp(-x)
  }
  return(sum / N)
}


N <- 100000
val1 <- log(3)/4
estimated_value <- f1(N)
absolute_error <- abs(val1 - estimated_value)
relative_error <- absolute_error / val1

cat("Valoarea estimată a integralei:", estimated_value, "\n")
cat("Eroare absolută:", absolute_error, "\n")
cat("Eroare relativă:", relative_error, "%\n")