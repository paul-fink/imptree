#' @title Classification with Imprecise Probabilities
#' 
#' @description Access probability information of nodes
#' 
#' @param x An object of class \code{imptree} or \code{node_imptree},
#' respectively. See details.
#' @param idx numeric or integer vector of indices specifying
#' the sequential node access from the root node.
#' Numeric values are coerced to integer as
#' by \code{\link{as.integer}}
#' (and hence truncated towards zero). \cr
#' If \code{NULL} the probability information of 
#' the root node are accessed.
#' 
#' @return An object of class \code{node_imptree} containing 
#' information on the properties of the node as a list:
#' \item{probint}{matrix containing the bounds of the imprecise
#' probability distribution and the absolute observed frequencies
#' of the classification variable within the node.}
#' \item{depth}{The depth of the node with the tree.}
#' \item{splitter}{The name of the variable used for splitting
#' as character; \code{NA} if node is a leaf.}
#' \item{children}{The number of children of the node.}
#' \item{traindataIdx}{Vector giving the indexes of the 
#' training data contained within the node}
#' \item{ipmodel}{List giving details about the used 
#' imprecise probability model to obatin the credal set:
#' \describe{
#' \item{iptype}{used IP model:
#' \code{"IDM"}, \code{"NPI"} or \code{"NPIapprox"}}
#' \item{s}{If \code{iptpye == "IDM"} the IDM's parameter 's',
#' otherwise this list entry is missing}
#' }}
#' 
#' @details
#' This function acceses the properties of a specific node 
#' of an imprecise tree. 
#' An existence check on the stored C++ object reference is 
#' carried out at first. If the reference is not valid the 
#' original call for \code{"x"} is printed as error.
#' 
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{imptree}}, for global information on 
#' the generated tree \code{\link{summary.imptree}}
#' 
#' @keywords tree
#' 
#' @export
node_imptree <- function(x, idx = NULL)  {
  # Are the C++ object references still stored in the R object?
  tryCatch({hasRoot_cpp(x$tree)},  error = function(e) {
	  stop(sprintf("reference to tree is not valid; see element \"call\" of '%s' for recreation", 
	               deparse(substitute(x))))
  })
  if(is.null(idx)) {
    message("extracting probability information from root node")
  } else if(!is.numeric(idx) || !is.null(dim(idx)) || any(idx < 1)) {
    stop("'idx' needs to be a positive integer vector or NULL")
  }
  res <- getNode_cpp(x$tree, as.integer(c(1, idx) - 1))
  # Adjust the indices for ranges used in R (i.e. starting by 1)
  res$traindataIdx <- res$traindataIdx + 1
  class(res) <- "node_imptree"
  res
}

#' @rdname node_imptree
#' @param ... Further arguments passed to \code{print} methods
#' @return The printing function returns the
#' \code{node_imptree} object invisibly.
#' @export
print.node_imptree <- function(x, ...) {
  cat(
    if(x$depth == 0L && x$children == 0L) {
      gettext("Root node (leaf)")
    } else if( x$children == 0L) {
      gettextf("Node (leaf) at level %d", 
               as.integer(x$depth))
    } else if(x$depth == 0L) {
      gettextf("Node with %d sub-nodes", 
               as.integer(x$children))
    } else {
      gettextf("Node with %d sub-nodes at level %d", 
               as.integer(x$children), as.integer(x$depth))
    }, "\n\n", sep = "")
  if(!is.na(x$splitter)) {
    cat(gettext("Splitting variable"), 
        ": ", x$splitter, "\n\n", sep = "")
  }
  cat(gettext("Probability Information:\n"))
  cat("\t", gettext("Model"), ": ", x$ipmodel$iptype, 
      if(!is.null(x$ipmodel$s)) {
        c(" (s=", format(x$ipmodel$s, ...), ")")
      } else {
        NULL
      }, "\n", sep = "")
  cat(gettext("\tTable for classification variable:\n"))
  print(x$probint, ...)
  invisible(x)
}

#' @rdname node_imptree
#' @export
`[[.imptree`  <- function(x, idx = NULL) {
  node_imptree(x, idx)
}
