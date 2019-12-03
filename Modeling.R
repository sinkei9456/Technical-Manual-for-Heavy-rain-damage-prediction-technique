# tic <- Sys.time()

# Required packages
# install.packages("doParallel")
# install.packages("caret")
# install.packages("glmnet")
# install.packages("randomForest")
# install.packages("gbm")
# install.packages("mbest")
# install.packages("PRROC")
# install.packages("dplyr")
# install.packages("ggplot2")


# Loading packages
library(doParallel)
library(caret)
library(glmnet)
library(randomForest)
library(gbm)
library(mbest)
library(PRROC)
library(dplyr)
library(ggplot2)


# Functions
source("Rfun/xxfun.R")
source("Rfun/pcfun.R")
source("Rfun/cvfun.R")
source("Rfun/clreg.R")
source("Rfun/dvfun.R")


# Loading dataset for model development
dat <- read.csv(
  file = "Dat_train/tdat.csv",
  header = TRUE,
  fileEncoding = "EUC-KR"
)


# Adding extra features
dat$beg_date <- as.Date(dat$beg_date, tz = "KST")
dat$end_date <- as.Date(dat$end_date, tz = "KST")
dat$ndate <- as.numeric(dat$end_date - dat$beg_date + 1)
dat$year <- as.numeric(substr(dat$beg_date, start=1, stop=4))
dat <- makedat(mydata = dat)
dat$sgg_code <- as.factor(x = dat$sgg_code)


# Outlier elimination
thresholds <- quantile(
  x = dat$y, 
  probs = c(
    0.36,  # lower bound
    1.00   # upper bound
  )
)
subsetindx <- (dat$y >= thresholds[1]) & (dat$y <= thresholds[2])
dat <- dat[subsetindx==T, , drop = F];
# head(dat); dim(dat); sum(complete.cases(dat)); table(dat$year);


# Validation
print("validation")
tdat <- dat[(dat$year <= (max(dat$year)-1)), , drop=F]
idat <- dat[(dat$year %in% (max(dat$year))), , drop=F] 
# dim(tdat); table(tdat$year);
# dim(idat); table(idat$year);

obj1 <- dev(dataset = tdat[tdat$region==1,,drop=F], mc = 2, sido = T)  # mc: the number of threads
obj2 <- dev(dataset = tdat[tdat$region==2,,drop=F], mc = 2, sido = F)
obj3 <- dev(dataset = tdat[tdat$region==3,,drop=F], mc = 2, sido = F)
obj4 <- dev(dataset = tdat[tdat$region==4,,drop=F], mc = 2, sido = F)

pout1 <- pred_out(object = obj1, pdat = idat[idat$region==1,,drop=F])
pout2 <- pred_out(object = obj2, pdat = idat[idat$region==2,,drop=F])
pout3 <- pred_out(object = obj3, pdat = idat[idat$region==3,,drop=F])
pout4 <- pred_out(object = obj4, pdat = idat[idat$region==4,,drop=F])

hout1 <- hval_out(pout = pout1, vdat = idat[idat$region==1,,drop=F])
hout2 <- hval_out(pout = pout2, vdat = idat[idat$region==2,,drop=F])
hout3 <- hval_out(pout = pout3, vdat = idat[idat$region==3,,drop=F])
hout4 <- hval_out(pout = pout4, vdat = idat[idat$region==4,,drop=F])

# the best models
optm <- c(
  region1 = hout1$best,
  region2 = hout2$best,
  region3 = hout3$best,
  region4 = hout4$best
)
saveRDS(object = optm, file = "Robj/optm.rds")

sink(file = "Output/validation.txt")
cat("\n")
print("region1")
hout1$vres
cat("\n")
print("region2")
hout2$vres
cat("\n")
print("region3")
hout3$vres
cat("\n")
print("region4")
hout4$vres
cat("\n")
print("The bests:")
optm
sink()

# barplots of Validated-RMSE
pdf(file = "Output/validation.pdf", width=9, height=6, onefile=T)
hout1$plt; hout2$plt; hout3$plt; hout4$plt;
dev.off()

# prediction and coverage in validation set
hout <- rbind.data.frame(
  hout1$vout,
  hout2$vout,
  hout3$vout,
  hout4$vout
)
pint <- pint_mm(
  hlist = list(
    region1 = hout1,
    region2 = hout2,
    region3 = hout3,
    region4 = hout4
  )
)
pdf(
  file = paste0("Output/coverage.pdf"),
  width = 9,
  height = 6,
  onefile = T
)
pint$plot_raw; pint$plot_log;
dev.off()

ptmp <- cbind.data.frame(
  hout[,c(1,2,3,4,5)],
  t(pint$pred)
)
pobj <- split(
  x = ptmp[,!colnames(ptmp)%in%c("region","sgg_code","year","beg_date","end_date")], 
  f = ptmp$region
)
pred <- cbind.data.frame(
  ptmp,
  pred_lwr = pint$pred_lwr,
  pred_upr = pint$pred_upr,
  pred_fit = do.call(
    what = "c", 
    args = lapply(
      X = as.list(1:4), 
      FUN = function(x) {
        pobj[[x]][,colnames(pobj[[x]])%in%optm[x]]
      }
    )
  ),
  y = pint$yact,
  cover = ifelse(
    test = (pint$pred_lwr <= pint$yact) & (pint$yact <= pint$pred_upr), 
    yes = 1, 
    no = 0
  )
)
rownames(pred) <- NULL

write.table(
  x = pred,
  file = "Output/validation.csv",
  sep = ",", 
  qmethod = "double", 
  row.names = F, 
  fileEncoding = "EUC-KR"
)


# Modeling each region's predictive model
print("re-fitting")
obj1 <- dev(dataset = dat[dat$region==1,,drop=F], mc = 2, sido = T)
obj2 <- dev(dataset = dat[dat$region==2,,drop=F], mc = 2, sido = F)
obj3 <- dev(dataset = dat[dat$region==3,,drop=F], mc = 2, sido = F)
obj4 <- dev(dataset = dat[dat$region==4,,drop=F], mc = 2, sido = F)

saveRDS(object = obj1, file = "Robj/obj1.rds")
saveRDS(object = obj2, file = "Robj/obj2.rds")
saveRDS(object = obj3, file = "Robj/obj3.rds")
saveRDS(object = obj4, file = "Robj/obj4.rds")

# End..

# toc <- Sys.time()
# toc - tic