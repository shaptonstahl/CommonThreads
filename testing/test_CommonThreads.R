# Test CommonThreads function

source('~/GitHub/CommonThreads/Rproject/dev/CommonThreads/R/CommonThreads.R')
library(dummies)

data(iris)

test.X <- subset(iris, select = -Species)
test.members <- (iris$Species == 'versicolor')
test.res <- CommonThreads(test.X, test.members)

test.res$threads


# Test dummy.data.frame
X <- data.frame(a=sample(letters, 100, TRUE))
X.dummies <- dummy.data.frame(X, sep='.')
y <- rnorm(100)
summary(lm(y ~ ., data=X.dummies))


# Demo common threads
n <- 100
test.X <- as.data.frame(matrix(rnorm(n*10), ncol=10))
test.members <- sample(1:n, 5)

test.res <- CommonThreads(test.X, test.members)
test.res$threads

