checkParam.IDM <- function(args) {

  if(missing(args) || is.null(args)) {
    args <- list(s = 1, correction = "no")
  } else if (is.list(args)) {
    if (is.null(names(args))) stop("The args list must have names")
    temp <- pmatch(names(args), c("s", "correction"), nomatch = 0L)
    if (any(temp == 0L)) {
      stop(gettextf("'method.param' component not matched: %s",
        names(args)[temp == 0L]), domain = NA)
    }
    names(args) <- c("s", "correction")[temp]
    if (is.null(args$s)) {
      temp <- 1
    } else {
      temp <- args$s
      if (temp < 0) stop("IDM value 's' must be >= 0")
    }
    if (is.null(args$correction)) {
      temp2 <- "no"
    } else {
      temp2 <- args$correction
      temp2 <- match.arg(temp2, c("no", "abellan", "strobl"))
    }
    args <- list(s = temp, correction = temp2)
  } else {
    stop("Parameter argument must be a list or null for default values")
  }
  args
}

checkParam.NPI <- function(args) {
  if(missing(args) || is.null(args)) {
    args <- list(maxEntropyMethod = "approx", correction = "no")
  } else if(is.list(args)) {
    if (is.null(names(args))) stop("The args list must have names")
    temp <- pmatch(names(args), c("maxEntropyMethod", "correction"), nomatch = 0L)
    if (any(temp == 0L)) {
      stop(gettextf("'method.param' component not matched: %s",
        names(args)[temp == 0L]), domain = NA)
    }
    names(args) <- c("maxEntropyMethod", "correction")[temp]
    if (is.null(args$maxEntropyMethod)) {
      temp <- "approx"
    } else {
      temp <- args$maxEntropyMethod
      temp <- match.arg(temp, c("approx", "exact"))
    }
    if (is.null(args$correction)) {
      temp2 <- "no"
    } else {
      temp2 <- args$correction
      temp2 <- match.arg(temp2, c("no", "strobl"))
    }
    args <- list(maxEntropyMethod = temp, correction = temp2)
  } else {
    stop("Parameter argument must be a list or null for default values")
  }
  args
}
