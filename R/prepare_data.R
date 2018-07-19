# Prepare data by given formula or imptree object
# In case of supplied imptree object its formula is used for creating the
# metadata of the Dataset object  
prepare_data <- function(object, data, weights, subset, ...) {
  
  # constructing a data.frame according to the supplied formula and na.action
  if(missing(object) || (!inherits(object, "imptree")  && !inherits(object, "formula"))) {
    stop("Either a valid formula or an imptree object is required")
  }
  if(!inherits(data, "data.frame")) {
    stop("'data' is required to be a data.frame object")
  }
  isTree <- inherits(object, "imptree")
  Call <- match.call()
  m <- match(c("data", "subset"), names(Call), 0L)
  mfCall <- Call[c(1L, m)]
  mfCall$formula <- if(isTree) {
    object$formula
  } else {
    object
  }
  mfCall$na.action <- na.fail
  mfCall[[1L]] <- as.name("model.frame")
  mf <- eval(mfCall, parent.frame())
  if (any(attr(attr(mf, "terms"), "order") > 1L)) {
    stop("Trees cannot handle interaction terms")
  }
  
  if(!all(sapply(mf, is.factor))) {
    stop("all variables in the resulting model frame need to be of class 'factor'")
  }
  
  if(missing(weights) || !(length(weights)>0)) {
    wt <- rep(1, nrow(mf))
  } else if(any(weights < 0)) {
    stop("Negative weights not allowed")
  } else {
    wt <- weights
  }

  cpp_data <- as.matrix(data.frame(lapply(mf, as.integer))) - 1
  storage.mode(cpp_data) <- "integer"
  attr(cpp_data, "nlevels") <- sapply(mf, nlevels)
  attr(cpp_data, "labels") <- lapply(mf, levels)
  attr(cpp_data, "classidx") <- 0
  attr(cpp_data, "wt") <- wt
  
  cpp_data
}