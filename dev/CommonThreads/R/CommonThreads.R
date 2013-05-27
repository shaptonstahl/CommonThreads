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
