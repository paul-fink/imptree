paste0 <- function(..., collapse = NULL) {
  paste(..., sep = "", collapse = collapse)
}


dyn.load(paste0(file.path("..", "src", "entropyIDM"), .Platform$dynlib.ext))
dyn.load(paste0(file.path("..", "src", "entropyNPI"), .Platform$dynlib.ext))
dyn.load(paste0(file.path("..", "src", "predict"), .Platform$dynlib.ext))

source("accuracy.r")
source("checkParam.r")
source("entropyIDM.r")
source("entropyNPI.r")
source("imptree_control.r")
source("predclass.r")
source("predict.r")
source("print.r")
source("treebuilder.r")
source("utils.r")
