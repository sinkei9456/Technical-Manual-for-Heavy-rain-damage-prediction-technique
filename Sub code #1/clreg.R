


# training clreg object
clreg <- function (
  
  # target's name
  target = colY,  
  
  # input's name
  inputs = col0,  
  
  # input's name for pc 
  pcomps = list(       
    pc1 = col1,
    pc2 = col2,
    pc3 = col3,
    pc4 = col4
  ),
  
  # threshold for pc
  thresh = list(       
    pc1 = 0.9,
    pc2 = 0.9,
    pc3 = 0.9,
    pc4 = 0.9
  ),
  
  # y-quantiles for cluster
  ycut = list(
    lwr = 0.10,
    upr = 0.90,
    int = 0.05
  ),
  
  # mean-formulas
  mufuns = list(
    lwr = "x1+x2",  
    upr = "x1*x2"
  ),
  
  # dataset (training/validation)
  data = list(
    dat_tr,  
    dat_te  
  ),
  
  # cv-ROC/cv-PR curves
  cvplot = F
  
) {
  
  
  # setting dataset
  dat_tr <- data$dat_tr
  dat_te <- data$dat_te
  
  
  # quantiles for reponse
  yval <- seq.default(
    from = ycut$lwr, to = ycut$upr, by = ycut$int
  )
  ycut <- quantile(
    c(dat_tr$y, dat_te$y), 
    probs = yval
  )
  ytab <- lapply(
    X = as.list(yval),
    FUN = function(x) {
      ycut <- quantile(x = dat_tr$y, probs = x)
      ybin <- ifelse(dat_tr$y >= ycut, 1, 0) 
      table(ybin)
    }
  )
  names(ytab) <- yval
  ycut <- ycut[sapply(ytab, length)==2]
  
  
  # pcut optim
  # print("pcut optim")
  {
    pauc.all <- pcut.all <- NULL
    for(i in 1:length(ycut)) {
      # print( paste(names(ycut[i]), "quantile") )
      
      ybin <- ifelse(dat_tr$y >= ycut[i], 1, 0) 
      
      set.seed(i)
      obs_kcv  <- cv.folds(n = nrow(dat_tr), folds = 10)
      
      # k-fold CV-AUC
      prob <- yact <- NULL
      for(j in 1:length(obs_kcv)) {
        # print(j)
        
        # pre-process for principal components
        pre_x <- mapply(
          FUN = function(pc, th) {
            preProcess(
              x = dat_tr[-obs_kcv[[j]], colnames(dat_tr) %in% pc],
              # centering/scaling/zero-var/near zero-var/pca
              method = c("center", "scale", "zv", "nzv", "pca"),  
              # proportion of variation
              thresh = th
            )
          },
          pcomps,
          thresh,
          SIMPLIFY = F
        )
        
        # concatenating pc 
        trn_x <- mapply(
          FUN = function(pre, pc) {
            predict( 
              object = pre, 
              newdata = dat_tr[-obs_kcv[[j]], colnames(dat_tr) %in% pc]
            )
          },
          pre_x,
          pcomps, 
          SIMPLIFY = F
        )
        trn_x <- do.call(
          what = "cbind", 
          args = trn_x
        )
        
        val_x <- mapply(
          FUN = function(pre, pc) {
            predict( 
              object = pre, 
              newdata = dat_tr[+obs_kcv[[j]], colnames(dat_tr) %in% pc]
            )
          },
          pre_x,
          pcomps, 
          SIMPLIFY = F
        )
        val_x <- do.call(
          what = "cbind", 
          args = val_x
        )
        
        
        # concatenating covariates 
        dat_bin_tr <- cbind.data.frame(
          ybin = ybin[-obs_kcv[[j]]],
          dat_tr[-obs_kcv[[j]], colnames(dat_tr) %in% inputs],
          trn_x
        )
        dat_bin_te <- cbind.data.frame(
          ybin =  ybin[+obs_kcv[[j]]],
          dat_tr[+obs_kcv[[j]], colnames(dat_tr) %in% inputs],
          val_x
        )
        
        # Logistic regression with firth's correction
        bin.f1 <- as.formula(
          paste0(paste0("  ybin ~ ", mufuns$lwr, " + ", paste0(colnames(trn_x), collapse = " + ") ))
        )
        bin.f2 <- as.formula(
          paste0(paste0("  ybin ~ ", mufuns$upr, " + ", paste0(colnames(trn_x), collapse = " + ") ))
        )
        
        f <- glm(
          formula = bin.f1,
          family = "binomial",
          data = dat_bin_tr,
          method = "firthglm.fit"
        )
        g <- glm(
          formula = bin.f2,
          family = "binomial",
          data = dat_bin_tr,
          method = "firthglm.fit"
        )
        h <- tryCatch(
          expr = {
            step(
              object = f,
              scope = list(
                lower = bin.f1,
                upper = bin.f2
              ),
              data = dat_bin_tr,
              direction = "both",
              trace = 0
            )
          }, 
          error = function(e) NULL
        )
        if(is.null(h)) h <- f
        
        
        # prediction for validation set
        prob[[j]] <- predict.glm(
          object = h, 
          newdata = dat_bin_te, 
          type = "response"
        )
        yact[[j]] <- dat_bin_te$ybin
      }
      
      ## aggregating p-probs and yact in validated set
      yact <- unlist(yact)
      yhat <- unlist(prob)
      
      p1 <- yhat[yact == 1]
      p0 <- yhat[yact == 0]
      
      # scores.class0: positive class, scores.class1: negative class
      cr1 <- PRROC::roc.curve(scores.class0 = p1, scores.class1 = p0, curve = T)
      cr2 <- PRROC:: pr.curve(scores.class0 = p1, scores.class1 = p0, curve = T)
      
      ## threshold 
      id1 <- which.max((1 - cr1$curve[,1]) + cr1$curve[,2])
      id2 <- which.max((2*cr2$curve[,1]*cr2$curve[,2])/(cr2$curve[,1]+cr2$curve[,2]))
      
      th1 <- cr1$curve[,3][id1]
      th2 <- cr2$curve[,3][id2]
      
      cm1 <- cv_cm(actual = yact, predicted = yhat, threshold = th1)
      cm2 <- cv_cm(actual = yact, predicted = yhat, threshold = th2)
      
      ## plotting output
      if(cvplot == T) {
        
        par(mfrow=c(2,1))
        
        # ROC curve
        plot(
          x = cr1$curve[,1], 
          y = cr1$curve[,2], 
          type = "l", 
          xlab = "1-specificity", 
          ylab = "sensitivity",
          xlim = c(0, 1),
          ylim = c(0, 1),
          main = paste("cv-AUROC: ", round(cr1$auc,4)),
          cex.main = 1
        )
        points(
          x = cr1$curve[,1][id1],
          y = cr1$curve[,2][id1],
          col = 2, pch = 20
        )
        mtext(
          bquote(
            paste(
              thresh==.(round(cm1$st["th"],4)),', ',
              acc ==.(round(cm1$st["acc"], 4)),', ',
              spec==.(round(cm1$st["spec"],4)),', ',
              rec==.(round(cm1$st["rec"],4)),', ',
              prec==.(round(cm1$st["prec"],4)),', ',
              f1 ==.(round(cm1$st["f1"], 4))
            )
          ), line=0.3, side=3, cex=0.9
        )
        
        # PR curve
        plot(
          x = cr2$curve[,1], 
          y = cr2$curve[,2], 
          type = "l", 
          xlab = "recall", 
          ylab = "precision",
          xlim = c(0, 1),
          ylim = c(0, 1),
          main = paste("cv-AUPR: ", round(cr2$auc.integral,4)),
          cex.main = 1
        )
        points(
          x = cr2$curve[,1][id2],
          y = cr2$curve[,2][id2],
          col = 2, pch = 20
        )
        mtext(
          bquote(
            paste(
              thresh==.(round(cm2$st["th"],4)),', ',
              acc ==.(round(cm2$st["acc"], 4)),', ',
              spec==.(round(cm2$st["spec"],4)),', ',
              rec==.(round(cm2$st["rec"],4)),', ',
              prec==.(round(cm2$st["prec"],4)),', ',
              f1 ==.(round(cm2$st["f1"], 4))
            )
          ), line=0.3, side=3, cex=0.9
        )
        par(mfrow=c(1,1))
      }
      
      pauc.all[[i]] <- c(roc = cr1$auc, pr = cr2$auc.integral)
      pcut.all[[i]] <- c(roc = cm1$st["th"], pr = cm2$st["th"])
    }
  }
  pcut <- sapply(X = pcut.all, FUN = function(x) x[1])
  names(pcut) <- names(ycut)
  pcut
  
  
  # ycut optim
  # print("ycut optim")
  {
    pout <- NULL
    
    for(i in 1:length(ycut)) {
      # print( paste(names(ycut[i]), "quantile") )
      
      ## 1. regression models for each group 
      ybin <- ifelse(dat_tr$y >= ycut[i], 1, 0) 
      dat_tr0 <- dat_tr[ybin==0,]
      dat_tr1 <- dat_tr[ybin==1,]
      
      # pre-process for principal components
      pre_x0 <- mapply(
        FUN = function(pc, th) {
          preProcess(
            x = dat_tr0[, colnames(dat_tr0) %in% pc],
            # centering/scaling/zero-var/near zero-var/pca
            method = c("center", "scale", "zv", "nzv", "pca"), 
            # proportion of variation
            thresh = th
          )
        },
        pcomps,
        thresh,
        SIMPLIFY = F
      )
      pre_x1 <- mapply(
        FUN = function(pc, th) {
          preProcess(
            x = dat_tr1[, colnames(dat_tr1) %in% pc],
            # centering/scaling/zero-var/near zero-var/pca
            method = c("center", "scale", "zv", "nzv", "pca"), 
            # proportion of variation
            thresh = th
          )
        },
        pcomps,
        thresh,
        SIMPLIFY = F
      )
      
      # concatenating pc 
      trn_x0 <- mapply(
        FUN = function(pre, pc) {
          predict( 
            object = pre, 
            newdata = dat_tr0[, colnames(dat_tr0) %in% pc]
          )
        },
        pre_x0,
        pcomps, 
        SIMPLIFY = F
      )
      trn_x0 <- do.call(
        what = "cbind", 
        args = trn_x0
      )
      
      trn_x1 <- mapply(
        FUN = function(pre, pc) {
          predict( 
            object = pre, 
            newdata = dat_tr1[, colnames(dat_tr1) %in% pc]
          )
        },
        pre_x1,
        pcomps, 
        SIMPLIFY = F
      )
      trn_x1 <- do.call(
        what = "cbind", 
        args = trn_x1
      )
      
      # concatenating covariates 
      dat_tr0 <- cbind.data.frame(
        y = dat_tr0$y,
        dat_tr0[, colnames(dat_tr0) %in% inputs],
        trn_x0
      )
      dat_tr1 <- cbind.data.frame(
        y = dat_tr1$y,
        dat_tr1[, colnames(dat_tr1) %in% inputs],
        trn_x1
      )
      
      
      # models for each group
      reg0.f1 <- as.formula(
        paste0(paste0("log(y) ~ ", mufuns$lwr, " + ", paste0(colnames(trn_x0), collapse = " + ") ))
      )
      reg0.f2 <- as.formula(
        paste0(paste0("log(y) ~ ", mufuns$upr, " + ", paste0(colnames(trn_x0), collapse = " + ") ))
      )
      f0 <- lm(
        formula = reg0.f1,
        data = dat_tr0
      )
      g0 <- lm(
        formula = reg0.f2,
        data = dat_tr0
      )
      h0 <- tryCatch(
        expr = {
          step(
            object = f0,
            scope = list(
              lower = reg0.f1,
              upper = reg0.f2
            ),
            data = dat_tr0,
            direction = "both",
            trace = 0
          )
        } , 
        error = function(e) NULL
      )
      if(is.null(h0)) h0 <- f0
      
      
      reg1.f1 <- as.formula(
        paste0(paste0("log(y) ~ ", mufuns$lwr, " + ", paste0(colnames(trn_x1), collapse = " + ") ))
      )
      reg1.f2 <- as.formula(
        paste0(paste0("log(y) ~ ", mufuns$upr, " + ", paste0(colnames(trn_x1), collapse = " + ") ))
      )
      f1 <- lm(
        formula = reg1.f1,
        data = dat_tr1
      )
      g1 <- lm(
        formula = reg1.f2,
        data = dat_tr1
      )
      h1 <- tryCatch(
        expr = {
          step(
            object = f1,
            scope = list(
              lower = reg1.f1,
              upper = reg1.f2
            ),
            data = dat_tr1,
            direction = "both",
            trace = 0
          )
        },
        error = function(e) NULL
      )
      if(is.null(h1)) h1 <- f1
      
      
      ## 2. logistic regression for classify the group 
      # pre-process for principal components
      pre_x <- mapply(
        FUN = function(pc, th) {
          preProcess(
            x = dat_tr[, colnames(dat_tr) %in% pc],
            # centering/scaling/zero-var/near zero-var/pca
            method = c("center", "scale", "zv", "nzv", "pca"), 
            # proportion of variation
            thresh = th
          )
        },
        pcomps,
        thresh,
        SIMPLIFY = F
      )
      
      # concatenating pc 
      trn_x <- mapply(
        FUN = function(pre, pc) {
          predict( 
            object = pre, 
            newdata = dat_tr[, colnames(dat_tr) %in% pc]
          )
        },
        pre_x,
        pcomps, 
        SIMPLIFY = F
      )
      trn_x <- do.call(
        what = "cbind", 
        args = trn_x
      )
      
      val_x <- mapply(
        FUN = function(pre, pc) {
          predict( 
            object = pre, 
            newdata = dat_te[, colnames(dat_te) %in% pc]
          )
        },
        pre_x,
        pcomps, 
        SIMPLIFY = F
      )
      val_x <- do.call(
        what = "cbind", 
        args = val_x
      )
      
      # concatenating covariates 
      dat_bin_tr <- cbind.data.frame(
        ybin = ybin,
        dat_tr[, colnames(dat_tr) %in% inputs],
        trn_x
      )
      dat_bin_te <- cbind.data.frame(
        dat_te[, colnames(dat_te) %in% inputs],
        val_x
      )
      
      
      # Logistic regression with firth's correction
      bin.f1 <- as.formula(
        paste0(paste0("  ybin ~ ", mufuns$lwr, " + ", paste0(colnames(trn_x), collapse = " + ") ))
      )
      bin.f2 <- as.formula(
        paste0(paste0("  ybin ~ ", mufuns$upr, " + ", paste0(colnames(trn_x), collapse = " + ") ))
      )
      
      f <- glm(
        formula = bin.f1,
        family = "binomial",
        data = dat_bin_tr,
        method = "firthglm.fit"
      )
      g <- glm(
        formula = bin.f2,
        family = "binomial",
        data = dat_bin_tr,
        method = "firthglm.fit"
      )
      h <- tryCatch(
        expr = {
          step(
            object = f,
            scope = list(
              lower = bin.f1,
              upper = bin.f2
            ),
            data = dat_bin_tr,
            direction = "both",
            trace = 0
          )
        }, 
        error = function(e) NULL
      )
      if(is.null(h)) h <- f
      
      # calculating for probs for group 1
      phat <- predict.glm(
        object = h, 
        newdata = dat_bin_te, 
        type = "response"
      )
      
      
      ## 3. prediction by group's regressions
      gr <- ifelse(phat >= pcut[i], 1, 0)
      dat_te0 <- dat_te[gr==0,,drop=F]
      dat_te1 <- dat_te[gr==1,,drop=F]
      
      
      if( (nrow(dat_te1)==0) ) {
        
        # concatenating pc 
        val_x0 <- mapply(
          FUN = function(pre, pc) {
            predict( 
              object = pre, 
              newdata = dat_te0[, colnames(dat_te0) %in% pc]
            )
          },
          pre_x0,
          pcomps, 
          SIMPLIFY = F
        )
        val_x0 <- do.call(
          what = "cbind", 
          args = val_x0
        )
        
        # concatenating covariates 
        dat_te0_tmp <- cbind.data.frame(
          y = dat_te0$y,
          dat_te0[, colnames(dat_te0) %in% inputs],
          val_x0
        )
        
        # predictions by group's regression
        pdat0 <- cbind.data.frame(
          dat_te0,
          pred = exp(  # for group "0"
            predict.lm(
              object = h0, 
              newdata = dat_te0_tmp
            ) 
          )  
        )
        
        pdat <- pdat0
        
      } else if( (nrow(dat_te0)==0) ) {
        
        # concatenating pc 
        val_x1 <- mapply(
          FUN = function(pre, pc) {
            predict( 
              object = pre, 
              newdata = dat_te1[, colnames(dat_te1) %in% pc]
            )
          },
          pre_x1,
          pcomps, 
          SIMPLIFY = F
        )
        val_x1 <- do.call(
          what = "cbind", 
          args = val_x1
        )
        
        # concatenating covariates 
        dat_te1_tmp <- cbind.data.frame(
          y = dat_te1$y,
          dat_te1[, colnames(dat_te1) %in% inputs],
          val_x1
        )
        
        # predictions by group's regression
        pdat1 <- cbind.data.frame(
          dat_te1,
          pred = exp(  # for group "1"
            predict.lm(
              object = h1, 
              newdata = dat_te1_tmp
            ) 
          )  
        )
        
        pdat <- pdat1
        
      } else {
        
        # concatenating pc 
        val_x0 <- mapply(
          FUN = function(pre, pc) {
            predict( 
              object = pre, 
              newdata = dat_te0[, colnames(dat_te0) %in% pc]
            )
          },
          pre_x0,
          pcomps, 
          SIMPLIFY = F
        )
        val_x0 <- do.call(
          what = "cbind", 
          args = val_x0
        )
        
        val_x1 <- mapply(
          FUN = function(pre, pc) {
            predict( 
              object = pre, 
              newdata = dat_te1[, colnames(dat_te1) %in% pc]
            )
          },
          pre_x1,
          pcomps, 
          SIMPLIFY = F
        )
        val_x1 <- do.call(
          what = "cbind", 
          args = val_x1
        )
        
        # concatenating covariates 
        dat_te0_tmp <- cbind.data.frame(
          y = dat_te0$y,
          dat_te0[, colnames(dat_te0) %in% inputs],
          val_x0
        )
        
        dat_te1_tmp <- cbind.data.frame(
          y = dat_te1$y,
          dat_te1[, colnames(dat_te1) %in% inputs],
          val_x1
        )
        
        # predictions by group's regression
        pdat0 <- cbind.data.frame(
          dat_te0,
          pred = exp(  # for group "0"
            predict.lm(
              object = h0, 
              newdata = dat_te0_tmp
            ) 
          )  
        )
        
        pdat1 <- cbind.data.frame(
          dat_te1,
          pred = exp(  # for group "1"
            predict.lm(
              object = h1, 
              newdata = dat_te1_tmp
            ) 
          )  
        )
        
        pdat <- rbind.data.frame(pdat0, pdat1)
        
      }
      
      
      ## 4. test prediction
      pdat <- pdat %>% arrange(sgg_code, year, beg_date, end_date)
      pdat$diff <- pdat$y - pdat$pred
      pout[[i]] <- pdat
      
    }
    
  }
  
  tune_ycut <- cbind.data.frame(
    ycut, 
    pauc = do.call("rbind", pauc.all)[,1], 
    pcut = do.call("rbind", pcut.all)[,1],
    rmse = sapply(X = pout, FUN = function(x) sqrt(mean((x$y - x$pred)^2))), 
    rsqr = sapply(
      X = pout, 
      FUN = function(x) ifelse(
        test = (is.null(x)), yes = NaN, no = cor(x$y, x$pred)^2
      )
    ),
    mape = sapply(X = pout, FUN = function(x) mean(abs((x$y - x$pred)/x$y)))
  )
  # tune_ycut
  
  
  # save thresholds
  i <- which.min(tune_ycut$rmse)
  ycut_opt <- tune_ycut[i,"ycut"]
  pcut_opt <- tune_ycut[i,"pcut"]
  
  
  # re-training using optimized cutoffs
  # print("re-fitting")
  {
    
    
    # 0. get tuned paramters
    clist <- c(ycut = ycut_opt, pcut = pcut_opt)
    
    dat <- rbind.data.frame(
      dat_tr,
      dat_te
    )
    ybin <- ifelse(dat$y >= ycut_opt, 1, 0)
    dat0 <- dat[ybin==0,]; dim(dat0);
    dat1 <- dat[ybin==1,]; dim(dat1);
    
    
    # 1. renew the group "0" regression
    pre_x0 <- mapply(
      FUN = function(pc, th) {
        preProcess(
          x = dat0[, colnames(dat0) %in% pc],
          # centering/scaling/zero-var/near zero-var/pca
          method = c("center", "scale", "zv", "nzv", "pca"), 
          # proportion of variation
          thresh = th
        )
      },
      pcomps,
      thresh,
      SIMPLIFY = F
    )
    
    trn_x0 <- mapply(
      FUN = function(pre, pc) {
        predict( 
          object = pre, 
          newdata = dat0[, colnames(dat0) %in% pc]
        )
      },
      pre_x0,
      pcomps, 
      SIMPLIFY = F
    )
    trn_x0 <- do.call(
      what = "cbind", 
      args = trn_x0
    )
    
    dat0 <- cbind.data.frame(
      y = dat0$y,
      dat0[, colnames(dat0) %in% inputs],
      trn_x0
    ) 
    
    reg0.f1 <- as.formula(
      paste0(paste0("log(y) ~ ", mufuns$lwr, " + ", paste0(colnames(trn_x0), collapse = " + ") ))
    )
    reg0.f2 <- as.formula(
      paste0(paste0("log(y) ~ ", mufuns$upr, " + ", paste0(colnames(trn_x0), collapse = " + ") ))
    )
    f0 <- lm(
      formula = reg0.f1,
      data = dat0
    )
    g0 <- lm(
      formula = reg0.f2,
      data = dat0
    )
    h0 <- tryCatch(
      expr = {
        step(
          object = f0,
          scope = list(
            lower = reg0.f1,
            upper = reg0.f2
          ),
          data = dat0,
          direction = "both",
          trace = 0
        )
      }, 
      error = function(e) NULL
    )
    if(is.null(h0)) h0 <- f0
    
    
    
    # 2. renew the group "1" regression
    pre_x1 <- mapply(
      FUN = function(pc, th) {
        preProcess(
          x = dat1[, colnames(dat1) %in% pc],
          # centering/scaling/zero-var/near zero-var/pca
          method = c("center", "scale", "zv", "nzv", "pca"), 
          # proportion of variation
          thresh = th
        )
      },
      pcomps,
      thresh,
      SIMPLIFY = F
    )
    
    trn_x1 <- mapply(
      FUN = function(pre, pc) {
        predict( 
          object = pre, 
          newdata = dat1[, colnames(dat1) %in% pc]
        )
      },
      pre_x1,
      pcomps, 
      SIMPLIFY = F
    )
    trn_x1 <- do.call(
      what = "cbind", 
      args = trn_x1
    )
    
    dat1 <- cbind.data.frame(
      y = dat1$y,
      dat1[, colnames(dat1) %in% inputs],
      trn_x1
    ) 
    
    reg1.f1 <- as.formula(
      paste0(paste0("log(y) ~ ", mufuns$lwr, " + ", paste0(colnames(trn_x1), collapse = " + ") ))
    )
    reg1.f2 <- as.formula(
      paste0(paste0("log(y) ~ ", mufuns$upr, " + ", paste0(colnames(trn_x1), collapse = " + ") ))
    )
    f1 <- lm(
      formula = reg1.f1,
      data = dat1
    )
    g1 <- lm(
      formula = reg1.f2,
      data = dat1
    )
    h1 <- tryCatch(
      expr = {
        step(
          object = f1,
          scope = list(
            lower = reg1.f1,
            upper = reg1.f2
          ),
          data = dat1,
          direction = "both",
          trace = 0
        )
      },
      error = function(e) NULL
    )
    if(is.null(h1)) h1 <- f1
    
    
    
    # 3. renew logistic regression
    pre_x <- mapply(
      FUN = function(pc, th) {
        preProcess(
          x = dat[, colnames(dat) %in% pc],
          # centering/scaling/zero-var/near zero-var/pca
          method = c("center", "scale", "zv", "nzv", "pca"), 
          # proportion of variation
          thresh = th
        )
      },
      pcomps,
      thresh,
      SIMPLIFY = F
    )
    
    trn_x <- mapply(
      FUN = function(pre, pc) {
        predict( 
          object = pre, 
          newdata = dat[, colnames(dat) %in% pc]
        )
      },
      pre_x,
      pcomps, 
      SIMPLIFY = F
    )
    trn_x <- do.call(
      what = "cbind", 
      args = trn_x
    )
    
    dat_bin <- cbind.data.frame(
      ybin = ybin,
      dat[, colnames(dat) %in% inputs],
      trn_x
    ) 
    
    bin.f1 <- as.formula(
      paste0(paste0("  ybin ~ ", mufuns$lwr, " + ", paste0(colnames(trn_x), collapse = " + ") ))
    )
    bin.f2 <- as.formula(
      paste0(paste0("  ybin ~ ", mufuns$upr, " + ", paste0(colnames(trn_x), collapse = " + ") ))
    )
    
    f <- glm(
      formula = bin.f1,
      family = "binomial",
      data = dat_bin,
      method = "firthglm.fit"
    )
    g <- glm(
      formula = bin.f2,
      family = "binomial",
      data = dat_bin,
      method = "firthglm.fit"
    )
    h <- tryCatch(
      expr = {
        step(
          object = f,
          scope = list(
            lower = bin.f1,
            upper = bin.f2
          ),
          data = dat_bin,
          direction = "both",
          trace = 0
        )
      },
      error = function(e) NULL
    )
    if(is.null(h)) h <- f
    
    
    
    # 4. models' coeff
    hlist <- list(
      logistic = h,
      reg0 = h0,
      reg1 = h1
    )
    
    
    
    # 5. pca's objects
    pcl <- lapply(
      X = pre_x, 
      FUN = function(x) {
        list(
          stat = cbind(
            mean = x$mean,
            std  = x$std
          ),
          coef = as.matrix(
            x$rotation
          )
        )
      }
    )
    names(pcl) <- sprintf("gr%d", 1:length(pcl))
    
    pc0 <- lapply(
      X = pre_x0, 
      FUN = function(x) {
        list(
          stat = cbind(
            mean = x$mean,
            std  = x$std
          ),
          coef = as.matrix(
            x$rotation
          )
        )
      }
    )
    names(pc0) <- sprintf("gr%d", 1:length(pc0))
    
    pc1 <- lapply(
      X = pre_x1, 
      FUN = function(x) {
        list(
          stat = cbind(
            mean = x$mean,
            std  = x$std
          ),
          coef = as.matrix(
            x$rotation
          )
        )
      }
    )
    names(pc1) <- sprintf("gr%d", 1:length(pc1))
    
    plist <- list(
      # reg "0"
      pc0 = pc0, 
      
      # reg "1"
      pc1 = pc1,
      
      # logistic
      pcl = pcl
    )
    
  }
  
  
  
  # 6. preProc's objects
  preps <- list(
    pc0 = pre_x0,
    pc1 = pre_x1,
    pcl = pre_x
  )
  
  
  # 7. output
  return(
    list(
      # tuning process
      tune_ycut = tune_ycut,
      # results
      clist = clist,    # cutoff
      hlist = hlist,    # reg-coeff
      plist = plist,    # pc-coef
      preps = preps,    # preProc
      # inform
      pcomps = pcomps   # input's name for pc 
    )
  )
  
}





# prediction from clreg object
predict.clreg <- function(object, newdata) {
  
  pdat <- newdata
  
  clist <- object$clist
  hlist <- object$hlist
  preps <- object$preps
  
  pcomps <- object$pcomps
  
  thresh <- lapply(
    X = preps[[1]], 
    FUN = function(x) x$thresh
  )
  
  
  # Step-1: Logistic reg
  input_x <- mapply(
    FUN = function(pre, pc) {
      predict( 
        object = pre, 
        newdata = pdat[, colnames(pdat) %in% pc]
      )
    },
    preps$pcl,
    pcomps, 
    SIMPLIFY = F
  )
  input_x <- do.call(
    what = "cbind", 
    args = input_x
  )
  
  idat <- cbind.data.frame(
    pdat,
    input_x
  )
  
  regl <- hlist$logistic
  prob <- predict(regl, newdata = idat, type = "response")
  pcut <- clist["pcut"]
  yvec <- ifelse(prob >= pcut, 1, 0)
  
  
  
  # Step-2: Regression
  # group "0"
  pdat0 <- pdat[yvec==0,]
  
  input_x0 <- suppressWarnings( 
    mapply(
      FUN = function(pre, pc) {
        predict( 
          object = pre, 
          newdata = pdat0[, colnames(pdat0) %in% pc]
        )
      },
      preps$pc0,
      pcomps, 
      SIMPLIFY = F
    )
  )
  input_x0 <- do.call(
    what = "cbind", 
    args = input_x0
  )
  
  idat0 <- cbind.data.frame(
    pdat0,
    input_x0
  )
  
  reg0 <- hlist$reg0
  exp0 <- suppressWarnings( exp( predict(reg0, newdata = idat0, type = "response") ) )
  
  
  # group "1"
  pdat1 <- pdat[yvec==1,]
  
  input_x1 <- suppressWarnings( 
    mapply(
      FUN = function(pre, pc) {
        predict( 
          object = pre, 
          newdata = pdat1[, colnames(pdat1) %in% pc]
        )
      },
      preps$pc1,
      pcomps, 
      SIMPLIFY = F
    )
  )
  input_x1 <- do.call(
    what = "cbind", 
    args = input_x1
  )
  
  idat1 <- cbind.data.frame(
    pdat1,
    input_x1
  )
  
  reg1 <- hlist$reg1
  exp1 <- suppressWarnings( exp( predict(reg1, newdata = idat1, type = "response") ) )
  
  
  
  # Step-3: Summary
  n0 <- length(exp0)
  n1 <- length(exp1)
  
  if( (n0!=0) & (n1==0) ) {
    
    odat0 <- cbind.data.frame(idat0, pred = exp0, gr = 0)
    delx0 <- grep(pattern = ".PC", x = colnames(odat0), value = T)
    # odat0 <- odat0[, !colnames(odat0) %in% delx0, drop = T]
    odat0 <- odat0[, !colnames(odat0) %in% delx0]
    
    odat  <- odat0
    
  } else if( (n0==0) & (n1!=0) ) {
    
    odat1 <- cbind.data.frame(idat1, pred = exp1, gr = 0)
    delx1 <- grep(pattern = ".PC", x = colnames(odat1), value = T)
    # odat1 <- odat1[, !colnames(odat1) %in% delx1, drop = T]
    odat1 <- odat1[, !colnames(odat1) %in% delx1]
    
    odat  <- odat1
    
  } else if( (n0!=0) & (n1!=0) ) {
    
    odat0 <- cbind.data.frame(idat0, pred = exp0, gr = 0)
    delx0 <- grep(pattern = ".PC", x = colnames(odat0), value = T)
    odat0 <- odat0[, !colnames(odat0) %in% delx0, drop = T]
    
    odat1 <- cbind.data.frame(idat1, pred = exp1, gr = 1)
    delx1 <- grep(pattern = ".PC", x = colnames(odat1), value = T)
    odat1 <- odat1[, !colnames(odat1) %in% delx1, drop = T]
    
    odat <- rbind.data.frame(
      odat0,
      odat1
    )
  }
  
  odat <- odat %>% arrange(sgg_code, beg_date)
  odat
  
}
# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- #

# End..


