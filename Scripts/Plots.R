library(ggplot2)
load("FishLengths.RData")
dat <- x

dat$Age <- as.factor(dat$Age)
ggplot(data = dat[!is.na(dat$Age),]) + geom_boxplot(aes(Age, Length, color = Age))
