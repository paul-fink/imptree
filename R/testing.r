# loading all necessary functions and shared objects
#source("init.r")

if(!file.exists("test.txt")) {
v0 <- as.factor(sample(c("1", "2", "3"), 1000, replace=TRUE))
v1 <- as.factor(sample(c("A", "B", "C"), 1000, replace=TRUE))
v2 <- as.factor(sample(c("D", "E", "F", "G"), 1000, replace=TRUE))
v3 <- as.factor(sample(c("H", "I", "J"), 1000, replace=TRUE))
v4 <- as.factor(sample(c("K", "L", "M", "N"), 1000, replace=TRUE))
v5 <- as.factor(sample(c("O", "P", "Q", "R"), 1000, replace=TRUE))
v6 <- factor(trunc(as.numeric(v0)+0.5+runif(1000)), labels=c("V","W","X","Y"))
levels(v6) <-c("V","W","X","Y")
x <- data.frame(v0,v1,v2,v3,v4,v5,v6)
write.table(x, file="test.txt", sep=",")
} else {
  x <- read.table(file="test.txt", header=TRUE, sep=",")
  x$v0 <- as.factor(x$v0)
}


car <- read.table(file="car_data.txt", header=TRUE, sep=",")
cartree1 <- imptree(class~., data = car)

res <- imptree(v0~., data=x, method.param=list(correction="strobl"))
res <- imptree(v0~., data=x)

b1 <- impbag(v0~., data=x)

xmiss <- rbind(x, c("S",NA,NA,NA,NA,"O","V","SS"))
ymiss <- rbind(x, c(NA,NA,NA,NA,NA,"O",NA,NA))

res<-imptree(v0~. , data=xmiss)
#
#b1 <- impbags(v0~., data=xmiss)

# function of comparing arguements
check.equals <- function(x, y) {
  if(length(x) != length(y)) return(FALSE)
  res <- logical(length(x))
  for(i in seq_along(x)) {
    res[i] <- if(is.na(x[i])) {
                is.na(y[i])
              } else if (is.nan(x[i])) {
                is.nan(y[i])
              } else if (is.na(y[i]) || is.nan(y[i])) {
                FALSE
              } else {
                x[i]==y[i]
              }
  }
  isTRUE(all(res))
}  

### testing of accuracy functions ####
test.accuracy <- function() {
  set.seed(12345)
  b1 <- impbag(v0~., data=x)
  grid <- expand.grid(agg = c("equal", "dacc", "disjunction", "mean"), dom = c("strong", "max"))
  acctest <- apply(grid, 1, function(x) {
    accuracy(b1, aggmethod=x[1], dominance = x[2])$acc
  })
# checking for list sizes
  if(!is.list(acctest) || length(acctest) != 8) stop("accuracy test failed (not list or not length 8)")
  if(any(sapply(acctest, function(x) length(x)!=5))) stop("accuracy test failed (not enough elements in each list)")
# checking for the names
  nam <- sapply(acctest, function(x) names(x))
  description = c("determinacy","singleacc","setacc","nindeterminant","discountedacc")
  if(any(sapply(seq_len(NROW(nam)), function(x) { any(nam[x,] != description[x]) } ))) stop("accuracy test failed (illegal row names)")
##### need to add test with actual values
  if(!check.equals(c(1,     0.695, NA, NA, 0.695,
                     1,     0.695, NA, NA, 0.695,
                     0.366, 1,     1,  2,  0.683,
                     1,     0.695, NA, NA, 0.695,
                     1,     0.695, NA, NA, 0.695,
                     1,     0.695, NA, NA, 0.695,
                     1,     0.708, NA, NA, 0.708, 
                     1,     0.695, NA, NA, 0.695), unlist(acctest))) stop("accuracy test faild (not correct values)")

  print("accuracy test succesful")
}
