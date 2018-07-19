#' @title Classification with Imprecise Probabilities
#' 
#' @description Printing the \code{imptree} object to console
#' 
#' @param x Object of class \code{imptree}. See details.
#' @param dominance Dominace criterion to be applied when predicting
#' classes. See \code{\link{predict.imptree}} for details.
#' @param \dots Additional arguments; ignored at the moment
#' 
#' @return Returns the calling object invisible.
#' 
#' @details
#' An existence check on stored Java Object References is carried out 
#' at first. If the Java Object References are not valid the original call
#' for \code{"object"} is printed as error.
#' 
#' For a more detailed summary of the tree \code{\link{summary.imptree}}.
#'
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{imptree}}, \code{\link{summary.imptree}}
#' 
#' @keywords tree
print.imptree <- function(x, dominance = c("strong", "max"), ...){
  cat("\nCall:  ", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
#  if(.jequals(x$tree, NULL)) {
#    stop(sprintf("Java reference to tree is not available.
#                 Please re-run tree creation!
#                 Tree call: %s", deparse(x$call)))
#  }
  
#  dominance <- match.arg(dominance)
#  domint <- pmatch(dominance, c("strong", "max")) - 1
  
#  cat("Tree structure:\n")
#  cat(.jcall("common/RInterface", "S", "printTree", 
#             x$tree, as.integer(domint)), "\n")
  invisible(x)
}