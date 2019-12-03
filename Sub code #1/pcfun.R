


## calculations of principal component 
calc_pc1 <- function(x_tr, x_te, ncomp) {
  
  preProc <- preProcess(
    x = x_tr,
    # centering/scaling/zero-var/near zero-var/
    method = c("center", "scale", "zv", "nzv", "pca"),  
    # thresh = 0.9   # proportion of variation
    pcaComp = ncomp  # number of principle component
  )
  
  # pc for training set
  pc_tr <- predict(
    preProc, 
    newdata = x_tr
  )  
  
  # pc for test set
  pc_te <- predict(
    preProc, 
    newdata = x_te
  ) 
  
  return( list(preProc = preProc, pc_tr = pc_tr,  pc_te = pc_te) )
}

calc_pc2 <- function(x, ncomp) {
  preProcess(
    x = x,
    # centering/scaling/zero-var/near zero-var/
    method = c("center", "scale", "zv", "nzv", "pca"),  
    # thresh = 0.9   # proportion of variation
    pcaComp = ncomp  # number of principle component
  )
}

# End..

