

library(ISLR)
library(tidyverse)

d <- as_tibble(ISLR::Default)

set.seed(312)
train_idx = sample(nrow(d), round(nrow(d)/2))

m <- glm(default ~ ., data = d, family = binomial, subset = train_idx)

#table(predict(m, data = d[-train_idx,], type = 'response'), d$default[-train_idx])


library(class)

train = iris[c(1:25, 51:75, 101:125), -5]
test = iris[c(26:50, 76:100, 126:150), -5]

cl = factor(c(rep("s", 25), rep("c", 25), rep("v", 25)))
f = knn(train, test, cl, k = 3)
table(f, cl)


P(L | R) = 0.6
P(L | ~R) = 0.1
P(R) = 0.3
P(R | L) = 0.6 * 0.3 / (0.6 * 0.3 + 0.1 * 0.7 )
# 0.72



