#' Checks to see if the elements of one vector/array are contained
#' in another vector/array
#' 
#' This is an aggregation of the "%in%" function.
#'
#' @param x vector or coercible to one
#' @param y vector or coercible to one
#' @return logical singleton
#' @export
#' @seealso \code{\link{CommonThreads}}
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
