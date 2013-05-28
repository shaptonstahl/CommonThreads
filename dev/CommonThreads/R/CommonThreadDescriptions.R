#' Generate plain text descriptions of the CommonThreads
#'
#' Uses the output of the CommonThreads function and describes those
#' common theads in plain text.
#' 
#' @param X output of CommonThreads
#' @return character vector with plain text desctions for each Thread in decreasing order of effect.
#' @export
#' @seealso \code{\link{CommonThreads}}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)
#' observations.selected <- sample(1:nrow(iris), 30)
#' result.ct <- CommonThreads(X=iris, members=observations.selected)
#' CommonThreadDescriptions(result.ct)
CommonThreadDescriptions <- function(X) {
  return(X)
}
                          