#' @title Classification with Imprecise Probabilities
#' 
#' @description Prediction of \code{imptree} objects
#' 
#' @param object An object of class \code{imptree}. See details.
#' @param data Data.frame containing observations to be predicted.
#' If \code{NULL} the observations in the training set of \code{"object"}
#' are employed. 
#' @param dominance Dominace criterion to be applied when predicting
#' classes. This may either be \code{"strong"} (default) or \code{"max"}.
#' See details.
#' @param utility Utility for the utility based accuracy measure for a 
#' vacuous prediction result (default: 0.65).
#' @param \dots Additional arguments for data. May be \code{"weights"},
#' \code{"subset"}, \code{"na.action"}, any further are discarded.
#' 
#' @return A named list containing predicted classes, predicted 
#' probability distribution and accuracy evaluation
#' \itemize{
#' \item{probintlist}{: List of the Imprecise Probability Distribution of the
#' class variable. One matrix per observation.}
#' \item{classes}{: Predicted class(es) of the observations as boolean matrix}
#' \item{evaluation}{: Result of accuracy evaluation
#' \itemize{
#' \item{nObs}{: Number of observations}
#' \item{deter}{: Determinacy}
#' \item{nObsIndet}{: Number of observations with indeterminate prediction}
#' \item{indetSize}{: Average number of classes when predicting 
#' indeterminate (\code{NA} when no indeterminate observation)}
#' \item{acc_single}{: Single-set accuracy (\code{NA} when no determinate 
#' observation)}
#' \item{acc_set}{: Set-accuracy (\code{NA} when no indeterminate observation)}
#' \item{acc_disc}{: Discounted-accuracy}
#' \item{acc_util}{: Utility based (discounted) accuracy}
#' }}
#' }
#' 
#' @details
#' This function carries out the prediction of an imprecise tree. 
#' An existence check on the stored C++ object reference is carried out 
#' at first. If the reference is not valid the original call
#' for \code{"object"} is printed as error.
#' 
#' There are currently 2 different dominance criteria available.
#' \itemize{
#' \item{\code{max}}{: Maximum frequency criterion. Dominance is decided only
#' by the upper bound of the probability interval, ie. a state \eqn{C_i} is 
#' dominated if there exists any \eqn{j \neq i}{j != i} with 
#' \eqn{u(C_i) < u(C_j)}}.
#' \item{\code{strong}}{: Interval dominance criterion. For the IDM it
#' coincides with the strong dominance criterion. Here a state
#' \eqn{C_i} is dominated if there exists any \eqn{j \neq i}{j != i} 
#' with \eqn{u(C_i) < l(C_j)}}
#' }
#' 
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{imptree}}
#' 
#' @keywords tree
#' 
#' @export
predict.imptree <- function(object, data, dominance = c("strong", "max"),
                            utility = 0.65, ...) {

  # Are the C++ object references still stored in the R object?
  tryCatch({hasRoot_cpp(object$tree)},  error = function(e) {
    stop(sprintf("Reference to tree is not valid!
                 Please re-run tree creation!
                 Tree call: %s", deparse(object$call)))
  })
  
  # checking for valid utility input
  if(0 >= utility || 1 <= utility) {
    stop("Utility needs to be in (0,1)")
  }
  
  # matching dominance type to C++ as int 
  dominance <- match.arg(dominance)
  domint <- pmatch(dominance, c("strong", "max")) - 1
  
  predcontrol <- list(utility = as.double(utility),
                      dominance = as.integer(domint))
  
  # In case of missing data, evaluation and predicting on training data
  if(missing(data)) {
    testdata <- object$traindata
  } else {
    testdata <- prepare_data(object, data = data, ...)
    
  }
  evaluation <- predict_cpp(object$tree, testdata, predcontrol)
  
  # Setting the classlabels on the prediction matrix and the Probinterval list
  classlabels  <- attr(object$data, "labels")[[1]]
  colnames(evaluation$classes) <- classlabels
  evaluation$probintlist <- lapply(evaluation$probintlist, function(o) {
    dimnames(o) <- list(c("upper", "lower"), classlabels)
    o
  })
  
  evaluation
}