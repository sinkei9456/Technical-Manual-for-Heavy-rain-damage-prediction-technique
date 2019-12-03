


## cv folds
cv.folds <- function (n, folds = 10) {
  split(sample(1:n), rep(1:folds, length = n))
}


## confusion matrix
cv_cm <- function(actual, predicted, threshold) {
  mat <- matrix(data = rep(NA,4), nrow = 2, ncol = 2)
  yact <- actual
  pred <- ifelse(
    test = predicted >= threshold,  
    yes  = 1, 
    no   = 0
  )
  mat[1,1] <- sum( (yact==0) & (pred==0) )
  mat[1,2] <- sum( (yact==0) & (pred==1) )
  mat[2,1] <- sum( (yact==1) & (pred==0) )
  mat[2,2] <- sum( (yact==1) & (pred==1) )
  dimnames(mat) <- list(actual = c("0", "1"), predicted = c("0", "1"))
  
  acc  <- sum(diag(mat))/sum(mat)
  spec <- mat[rownames(mat)=="0",colnames(mat)=="0"] / sum(mat[rownames(mat)=="0",])
  rec  <- mat[rownames(mat)=="1",colnames(mat)=="1"] / sum(mat[rownames(mat)=="1",])
  prec <- mat[rownames(mat)=="1",colnames(mat)=="1"] / sum(mat[,colnames(mat)=="1"])
  f1   <- (2 * rec * prec) / (rec + prec)
  
  return(list(
    cm = mat,
    st = c(
      th = threshold,
      acc = acc,
      spec = spec,
      rec = rec,
      prec = prec,
      f1 = f1
    )
  ))
}

# End..

