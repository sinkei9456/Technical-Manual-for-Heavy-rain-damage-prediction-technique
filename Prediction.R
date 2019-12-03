

# Pre-installed packages
# install.packages("doParallel")
# install.packages("caret")
# install.packages("glmnet")
# install.packages("randomForest")
# install.packages("gbm")
# install.packages("mbest")
# install.packages("PRROC")
# install.packages("dplyr")
# install.packages("ggplot2")


# Load package and function
library(doParallel)
library(caret)
library(mbest)
library(PRROC)
library(dplyr)
library(ggplot2)

source("Rfun/xxfun.R")
source("Rfun/pcfun.R")
source("Rfun/cvfun.R")
source("Rfun/clreg.R")
source("Rfun/dvfun.R")


# Loading input data for prediction
idat <- read.csv(  
  file = "Dat_input/idat.csv",
  header = TRUE,
  fileEncoding = "EUC-KR"
)


# Adding extra features
idat$beg_date <- as.Date(idat$beg_date, tz = "KST")
idat$end_date <- as.Date(idat$end_date, tz = "KST")
idat$ndate <- as.numeric(idat$end_date - idat$beg_date + 1)
idat$year <- as.numeric(substr(idat$beg_date, start=1, stop=4))
pdat <- makedat(mydata = idat)
pdat$sgg_code <- as.factor(x = pdat$sgg_code)
# head(pdat); dim(pdat); sum(complete.cases(pdat)); table(pdat$year);


# Loading model's object
obj1 <- readRDS(file = "Robj/obj1.rds")
obj2 <- readRDS(file = "Robj/obj2.rds")
obj3 <- readRDS(file = "Robj/obj3.rds")
obj4 <- readRDS(file = "Robj/obj4.rds")
optm <- readRDS(file = "Robj/optm.rds")


# Making prediction output
pout1 <- tryCatch(
  expr = suppressWarnings(pred_out(object = obj1, pdat = pdat[pdat$region==1,,drop=F])),
  error = function(e) NULL
)
pout2 <- tryCatch(
  expr = suppressWarnings(pred_out(object = obj2, pdat = pdat[pdat$region==2,,drop=F])),
  error = function(e) NULL
)
pout3 <- tryCatch(
  expr = suppressWarnings(pred_out(object = obj3, pdat = pdat[pdat$region==3,,drop=F])),
  error = function(e) NULL
)
pout4 <- tryCatch(
  expr = suppressWarnings(pred_out(object = obj4, pdat = pdat[pdat$region==4,,drop=F])),
  error = function(e) NULL
)
pout <- do.call(
  what = "rbind.data.frame", 
  args = mapply(
    FUN = function(pobj, mobj) {
      ptmp <- pobj[,!colnames(pobj)%in%c("region","sgg_code","year","beg_date","end_date")]
      cbind.data.frame(
        pobj,
        pred_lwr = apply(ptmp, 1, min),
        pred_upr = apply(ptmp, 1, max),
        pred_fit = ptmp[,colnames(ptmp) %in% mobj] 
      )
    },
    list(pout1, pout2, pout3, pout4),
    as.list(optm), 
    SIMPLIFY = F
  )
)


# Output 
write.table(
  pout,
  file = "Output/prediction.csv",
  sep = ",", 
  qmethod = "double", 
  row.names = F, 
  fileEncoding = "EUC-KR"
)

# End..

