# Test CommonThreads function

source('~/GitHub/CommonThreads/Rproject/dev/CommonThreads/R/CommonThreads.R')
library(dummies)

data(iris)

test.X <- subset(iris, select = -Species)
test.members <- (iris$Species == 'versicolor')
test.res <- CommonThreads(test.X, test.members)

test.res$threads


