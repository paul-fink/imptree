#' @title Classification with Imprecise Probabilities
#' 
#' @description Printing the \code{imptree} object to console
#' 
#' @param x Object of class \code{imptree}. See details.
#' @param digits a non-null value for digits specifies the minimum number
#' of significant digits to be printed in values. The default uses 
#' \code{\link{getOption}("digits")}. Non-integer values will be rounded down,
#' and only values greater than or equal to 1 and 
#' no greater than 17 are accepted.
#' @param sep Separator between the displayed IPDistribution objects.
#' (Default: \code{'\t'})
#' @param \dots Additional arguments; ignored at the moment
#' 
#' @return Returns the calling object invisible.
#' 
#' @details
#' An existence check on the stored C++ object reference is carried out 
#' at first. If the reference is not valid the original call
#' for \code{"object"} is printed as error.
#' 
#' For a more detailed summary of the tree \code{\link{summary.imptree}}.
#'
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{imptree}}, \code{\link{summary.imptree}}
#' 
#' @keywords tree
#' 
#' @export
print.imptree <- function(x, digits = getOption("digits"), sep = "\t", ...){
  
  cat("\nCall:  ", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  # Are the C++ object references still stored in the R object?
  tryCatch({hasRoot_cpp(x$tree)},  error = function(e) {
    stop(gettextf("reference to tree is not valid; see element \"call\" of '%s' for recreation", 
                 deparse(substitute(x)), domain = "R-imptree"))
  })
  digits <- floor(digits);
  if(digits < 1 || digits > 17) {
    stop("invalid 'digits' argument", domain = "R-imptree")
  }
  
  treePrint_cpp(x$tree, nsmall = digits, sep = sep)
  invisible(x)
}