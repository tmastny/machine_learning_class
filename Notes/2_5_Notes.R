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



# ---- 10-fold CV
Auto[complete.cases(Auto),]
set.seed(1)
rand <- sample(1:nrow(Auto))
Auto[rand,]
num_in_folds <- floor(nrow(Auto)/10)

temp <- data.frame(mse = 1:40)
for (j in 1:10) {
  for (i in 1:10) {
    m <- lm(mpg ~ poly(horsepower, j), data = Auto[-seq(num_in_folds * (i - 1) + 1, num_in_folds * i + 1),])
    temp[,1] <- (Auto[seq(num_in_folds * (i - 1) + 1, num_in_folds * i + 1), 'mpg'] - predict(m, newdata = Auto[seq(num_in_folds * (i - 1) + 1, num_in_folds * i + 1),]))^2
  }
  a[j, 1] <- mean(temp$mse)
}
a
plot(mse~poly, data = a)




