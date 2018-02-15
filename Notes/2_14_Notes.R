library(rpart)

spam = read.csv("http://thinktostart.com/data/data.csv",header=FALSE,sep=";")
names(spam) = read.csv("http://thinktostart.com/data/names.csv",
                       header=FALSE,sep=";",stringsAsFactors=FALSE)$V1
spam$y = factor(spam$y)

names(spam)[49:54] = paste0("char_freq_", 1:6)


