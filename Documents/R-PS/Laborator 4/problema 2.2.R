f1<- function(N, lambda) {
  sum = 0
  for (i in 1:N) {
    u = rexp(1, lambda)
    sum = sum + exp(-2*u^2)/exp(-u)
  }
  return(sum / N)
}

f2 = function(k, N, lambda) {
  estimates = 0
  for (i in 1:k) {
    estimates[i] = f1(N, lambda)
  }
  cat("Media estimărilor:", mean(estimates), "\n")
  cat("Deviatia standard:", sd(estimates), "\n")
}


N <- 50000
lambda <- 3
k <- 30
f2(k, N, lambda)


val1 <- sqrt(pi/8)
cat("Valoarea exactă a integralei:", val1, "\n")


estimated_value = f1(N, lambda)
abs_error = abs(exact_value - estimated_value)
rel_error = absolute_error / exact_value
print(estimated_value)
print(abs_error)
print(rel_error)