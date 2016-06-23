#
# function printing a object of class imptree
# Input:
#   x:  Object of class 'imptree'
#
# Printed output:
#   Call which generated the object
#   Accuracy achieved on the training set (if calucaleted in generation)
#   Accuracy achieved on the test set (if calucaleted in generation)
#   Employed splitting variables
#   strucutre of the tree
#

print.imptree <- function(x, ...){
  cat("\nCall:  ", paste(deparse(x$Call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if(!is.null(x$Accuracy)) {
    arr <- matrix(unlist(x$Accuracy$acc), dimnames = list(c("Determinacy", "Single set accuracy", "Set accuracy", "Number of indeterminant predictions", "Discounted accuracy"), ""))
    cat("\nAccuracy achieved on training data:\n")
    print(arr)
  }
  if(!is.null(x$Test.result)) {
    cat("\nAccuracy achieved on test data:")
    trr <- matrix(unlist(x$Test.result$acc), dimnames = list(c("Determinacy", "Single set accuracy", "Set accuracy", "Number of indeterminant predictions", "Discounted accuracy"), ""))
    print(trr)
  }
  cat("\n\nVariables used for splitting:", x$Splitnames)
  cat("\n\nImprecise Tree:\n\n")
  str(x$Tree)
}


#
# function printing a object of class impbag
# Input:
#   x:  Object of class 'impbag'
#
# Printed output:
#   Call which generated the object
#   Accuracy achieved on the training set (if calucaleted in generation)
#   Accuracy achieved on the test set (if calucaleted in generation)
#   strucutre of the tree grown on full training data set
#

print.impbag <- function(x, ...) {
  
  cat("\nCall:", paste(deparse(x$Call), sep = "\n", collapse = "\n"))
  if(!is.null(x$Accuracy)) {
    arr <- matrix(unlist(x$Accuracy$acc), dimnames = list(c("Determinacy", "Single set accuracy", "Set accuracy", "Number of indeterminant predictions", "Discounted accuracy"), ""))
    cat("\nAccuracy achieved on training data:\n")
    print(arr)
  }
  if(!is.null(x$Test)) {
    cat("\nAccuracy achieved on test data:\n")
    trr <- matrix(unlist(x$Test$acc), dimnames = list(c("Determinacy", "Single set accuracy", "Set accuracy", "Number of indeterminant predictions", "Discounted accuracy"), ""))
    print(trr)
  }
  cat("\n\nImprecise tree build on data:\n\n")
  str(x$Greedy.tree$Tree)
}
