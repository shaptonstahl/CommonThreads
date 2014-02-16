#' CommonThreads

# Remove detrius and start with a clean session
source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

#####  Load libraries  #####
source("http://www.haptonstahl.org/R/usePackage/usePackage.R")  # provides UsePackage
UsePackage("dummies")

#####  Set options  #####
# options(stringsAsFactors = FALSE)

#####  Load functions  #####

#####  DataFrameClasses  #####
#' Retrieve the class of each column in a data.frame
#'
#' A longer description of the function.  This can be perhaps
#' a paragraph, perhaps more than one.
#' 
#' @param X data.frame.
#' @return character vector with names of classes of each column of X
#' @export
#' @seealso \code{\link{Related function}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' DataFrameClasses(iris)
DataFrameClasses <- function(X) {
  # Guardians
  stopifnot(class(X) == "data.frame")
  
  # perform the function
  out <- sapply(1:ncol(X), function(i) class(X[,i]))
  
  # prepare and return the output
  return(out)
}
#' data(iris)   # provides example data
#' DataFrameClasses(iris)
#####

#####  is.subset  #####
#' Checks to see if the elements of one vector/array are contained
#' in another vector/array
#'
#' @param x vector or coercible to one
#' @param y vector or coercible to one
#' @return logical singleton
#' @export
#' @seealso \code{\link{CommonThreads}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' is.subset(1:3, 1:5)  # returns TRUE
#' is.subset(1:5, 1:3)  # returns FALSE
#' is.subset(array(1:9, dim=c(3,3)), c(1:100))  # returns TRUE
#' is.subset(matrix(1:200, ncol=2), array(1:125, dim=c(5,5,5)))  # returns FALSE
is.subset <- function(x, y) {
  # Guardians
  stopifnot(is(x, "vector"),
            is(y, "vector")
  )
  
  # deal with default and missing values
  
  # perform the function
  out <- all(x %in% y)
  # prepare and return the output
  return(out)
}
#####

#####  CommonThreads  #####
#' Which variables best explain membership?
#'
#' Identify which variables explain best why these observations/nodes
#' are distinct from those observations.
#' 
#' @param X data.frame of observations
#' @param members numeric vector of rows included or logical vector with TRUE for members
#' @param min.nonzero.certainty numeric p-value must be below 1-min.nonzero.certainty to report result for a coefficient
#' @return data.frame with variables column.name, effect, prob.not.zero
#' @export
#' @seealso \code{\link{CommonThreadDescriptions}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)
#' observations.selected <- sample(1:nrow(iris), 30)
#' CommonThreads(X=iris, members=observations.selected)
#' CommonThreads(X=iris, members=observations.selected, min.nonzero.certainty=.5)
CommonThreads <- function(X,
                          members,
                          min.nonzero.certainty=.75) {
  # Guardians
  stopifnot(is(X, "data.frame") && nrow(X) >= 2,
            (is(members, "logical") && length(members) == nrow(X)) || 
              (is(members, "numeric") && is.subset(members, 1:nrow(X))),
            min.nonzero.certainty > 0 && min.nonzero.certainty < 1
  )
  
  ###  deal with default and missing values  ###
  
  # Will work with Z internally
  Z <- dummy.data.frame(X, sep=".")
  Z <- scale(Z)
  
  # code dependent variable, taking out repeated elements
  dep.var.name <- paste("cty", paste(sample(letters, 10), collapse=""), sep=".")  # random name to avoid colision
  if( is(members, "logical") ) members <- which(members)
  members <- unique(members)  # remove duplicate indices
  dep.var <- rep(0, nrow(X))
  dep.var[members] <- 1
  Z <- data.frame(dep.var, Z)
  names(Z)[1] <- dep.var.name
  
  # perform the function
  probit.res <- glm(formula(paste(dep.var.name, "~ .")), 
                    data=Z, 
                    family=binomial(link="probit"))
  
  # prepare and return the output
  out <- summary(probit.res)$coefficients[-1,]
  out <- out[sort(abs(out[,1]), decreasing=TRUE, index.return=TRUE)$ix,]  # sort by absolute value of coefficient
  out <- out[out[,4] < 1 - min.nonzero.certainty,]
  out <- data.frame(rownames(out), out)
  names(out) <- c("column.name", "effect", "std.err", "z.value", "p.value")
  out$prob.not.zero <- 1 - out$p.value
  out <- out[,c("column.name", "effect", "prob.not.zero")]
  rownames(out) <- NULL
  
  return(out)
}
#####