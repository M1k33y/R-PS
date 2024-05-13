n <- 100000
x <- rgeom(n, 0.3)
y <- rgeom(n, 0.5)

p <- mean(x > y^2)

n <- 100
error <- 1
while (error > 0.005) {
  x <- c(x, rgeom(n, 0.3))
  y <- c(y, rgeom(n, 0.5))
  p_new <- mean(x > y^2)
  error <- qnorm(0.975)*sqrt(p_new*(1-p_new)/length(x))
  n <- n + 100 
}

cat("Estimated probability:", p, "\n")
cat("Number of runs:", length(x), "\n")