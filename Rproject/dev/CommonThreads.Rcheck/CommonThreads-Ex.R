pkgname <- "CommonThreads"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('CommonThreads')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CommonThreadDescriptions")
### * CommonThreadDescriptions

flush(stderr()); flush(stdout())

### Name: CommonThreadDescriptions
### Title: Generate plain text descriptions of the CommonThreads
### Aliases: CommonThreadDescriptions

### ** Examples

data(iris)
observations.selected <- sample(1:nrow(iris), 30)
result.ct <- CommonThreads(X=iris, members=observations.selected)
CommonThreadDescriptions(result.ct)



cleanEx()
nameEx("CommonThreads-package")
### * CommonThreads-package

flush(stderr()); flush(stdout())

### Name: CommonThreads-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: CommonThreads-package CommonThreads
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



cleanEx()
nameEx("CommonThreads")
### * CommonThreads

flush(stderr()); flush(stdout())

### Name: CommonThreads
### Title: Which variables best explain membership?
### Aliases: CommonThreads

### ** Examples

data(iris)
observations.selected <- sample(1:nrow(iris), 30)
CommonThreads(X=iris, members=observations.selected)
CommonThreads(X=iris, members=observations.selected, min.nonzero.certainty=.5)



cleanEx()
nameEx("is.subset")
### * is.subset

flush(stderr()); flush(stdout())

### Name: is.subset
### Title: Checks to see if the elements of one vector/array are contained
###   in another vector/array
### Aliases: is.subset

### ** Examples

is.subset(1:3, 1:5)  # returns TRUE
is.subset(1:5, 1:3)  # returns FALSE
is.subset(array(1:9, dim=c(3,3)), c(1:100))  # returns TRUE
is.subset(matrix(1:200, ncol=2), array(1:125, dim=c(5,5,5)))  # returns FALSE



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
