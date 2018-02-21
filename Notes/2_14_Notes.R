spam = read.csv("http://thinktostart.com/data/data.csv",header=FALSE,sep=";")
names(spam) = read.csv("http://thinktostart.com/data/names.csv",
                       header=FALSE,sep=";",stringsAsFactors=FALSE)$V1
spam$y = factor(spam$y)

names(spam)[49:54] = paste0("char_freq_", 1:6)


n = nrow(spam)
set.seed(98533795)
bootspam = spam[sample(n, n, replace = TRUE),]

library(rpart)
growntree = rpart.control(minsplit = 2, minbucket = 0, cp = 0)
f = rpart(y ~ ., data = spam, control = growntree)

mean(residuals(f))

table(predict(f, type = 'class'), spam$y)

#' ## Bootstrap
f = rpart(y ~ ., data = bootspam, control = growntree)
res = predict(f, newdata = spam, type = 'class')
table(res, spam$y)

mean(res!=spam$y)

res[968]
"968" %in% names(bootspam)


