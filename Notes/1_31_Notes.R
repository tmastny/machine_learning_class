library(ISLR)
data(Auto)

temp <- data.frame(mse = 1:nrow(Auto))

a <- data.frame(
  mse = 1:10,
  poly = 1:10
)

for (j in 1:10) {
  for (i in 1:nrow(Auto)) {
    m <- lm(mpg ~ poly(horsepower, j), data = Auto[-i,])
    temp[i,1] <- (Auto[i, 'mpg'] - predict(m, newdata = Auto[i,]))^2
  }
  a[j, 1] <- mean(temp$mse)
}
a
plot(mse~poly, data = a)
