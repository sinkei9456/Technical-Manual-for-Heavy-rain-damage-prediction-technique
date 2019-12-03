

# development code
dev <- function(
  dataset, 
  mc = 2,
  sido = F
) {
  
  # preprocess for pc 
  if(sido == T) {
    feature0 <- c(
      "sido_code", "ndate", "tot", "area"
    ) 
    clreg_lwr <- "sido_code + area + tot + ndate"
    clreg_upr <- "sido_code + area * tot + ndate"
    
  } else {
    feature0 <- c(
      "ndate", "tot", "area"
    )
    clreg_lwr <- "area + tot + ndate"
    clreg_upr <- "area * tot + ndate"
  }
  
  feature1 <- sprintf(
    "d%d", 7:1
  )
  feature2 <- sprintf(
    "x%d", 1:24
  )
  feature3 <- c(
    "grdp", "money", "property_tax", "building", "city_rate", "people", "vulnerable"
  )
  feature4 <- c(
    "recovery_cost", 
    "tlength",  "owgr", 
    sprintf("cnt%d", 1:4), sprintf("sum%d", 1:4), sprintf("avg%d", 1:4), 
    "ccnt", "ctpc", 
    "cDRZ", "nDRZ"
  )
  
  preProc1 <- preProcess(
    x = dataset[, colnames(dataset) %in% feature1, drop = F],
    method = c("center", "scale", "zv", "nzv", "pca"),  
    thresh = 0.9  
  )
  preProc2 <- preProcess(
    x = dataset[, colnames(dataset) %in% feature2, drop = F],
    method = c("center", "scale", "zv", "nzv", "pca"),  
    thresh = 0.9  
  )
  preProc3 <- preProcess(
    x = dataset[, colnames(dataset) %in% feature3, drop = F],
    method = c("center", "scale", "zv", "nzv", "pca"),  
    thresh = 0.9  
  )
  preProc4 <- preProcess(
    x = dataset[, colnames(dataset) %in% feature4, drop = F],
    method = c("center", "scale", "zv", "nzv", "pca"),  
    thresh = 0.9  
  )
  
  # pc-augmented dataset
  pc1 <- predict(object=preProc1, newdata=dataset[,colnames(dataset)%in%feature1, drop=F])
  pc2 <- predict(object=preProc2, newdata=dataset[,colnames(dataset)%in%feature2, drop=F])
  pc3 <- predict(object=preProc3, newdata=dataset[,colnames(dataset)%in%feature3, drop=F])
  pc4 <- predict(object=preProc4, newdata=dataset[,colnames(dataset)%in%feature4, drop=F])
  colnames(pc1) <- paste0(colnames(pc1), "_1")
  colnames(pc2) <- paste0(colnames(pc2), "_2")
  colnames(pc3) <- paste0(colnames(pc3), "_3")
  colnames(pc4) <- paste0(colnames(pc4), "_4")
  
  dat_0 <- dataset[, colnames(dataset) %in% c(
    "region", "sgg_code", "year", "beg_date", "end_date", 
    "y",
    feature0
  ), drop = F]
  dat_f <- cbind.data.frame(
    dat_0,
    pc1,
    pc2
  )
  dat_g <- cbind.data.frame(
    dat_0,
    pc1,
    pc2,
    pc3
  )
  dat_h <- cbind.data.frame(
    dat_0,
    pc1,
    pc2,
    pc3,
    pc4
  )
    
  # mean function
  mufun_f <- as.formula(
    paste0(
      "log(y) ~ ", 
      paste0(feature0, collapse = " + "),
      " + ", 
      paste0(colnames(pc1), collapse = " + "),
      " + ",
      paste0(colnames(pc2), collapse = " + ")
    )
  )
  mufun_g <- as.formula(
    paste0(
      "log(y) ~ ", 
      paste0(feature0, collapse = " + "),
      " + ", 
      paste0(colnames(pc1), collapse = " + "),
      " + ",
      paste0(colnames(pc2), collapse = " + "),
      " + ",
      paste0(colnames(pc3), collapse = " + ")
    )
  )
  mufun_h <- as.formula(
    paste0(
      "log(y) ~ ", 
      paste0(feature0, collapse = " + "),
      " + ", 
      paste0(colnames(pc1), collapse = " + "),
      " + ",
      paste0(colnames(pc2), collapse = " + "),
      " + ",
      paste0(colnames(pc3), collapse = " + "),
      " + ",
      paste0(colnames(pc4), collapse = " + ")
    )
  )
  
  # training control (for common use)
  set.seed(1)
  tctrl <- trainControl(
    method = "cv",
    number = 10,
    savePredictions = TRUE
  )
  
  # initiation
  f1 <- f2 <- f3 <- f4 <- f5 <- f6 <- NULL 
  g1 <- g2 <- g3 <- g4 <- g5 <- g6 <- NULL
  h1 <- h2 <- h3 <- h4 <- h5 <- h6 <- NULL
  

  print("lm")
  cl <- makeCluster(mc); registerDoParallel(cl);
  f1 <- suppressWarnings(
    train(
      mufun_f,
      data = dat_f,
      method = "lm",
      trControl = tctrl
    )
  )
  stopCluster(cl)
  
  cl <- makeCluster(mc); registerDoParallel(cl);
  g1 <- suppressWarnings(
    train(
      mufun_g,
      data = dat_g,
      method = "lm",
      trControl = tctrl
    )
  )
  stopCluster(cl)
  
  cl <- makeCluster(mc); registerDoParallel(cl);
  h1 <- suppressWarnings(
    train(
      mufun_h,
      data = dat_h,
      method = "lm",
      trControl = tctrl
    )
  )
  stopCluster(cl)
  
  
  print("glmnet")
  set.seed(1)
  cl <- makeCluster(mc); registerDoParallel(cl);
  f2 <- suppressWarnings(
    train(
      mufun_f,
      data = dat_f,
      method = "glmnet", 
      trControl = tctrl,
      preProc = c("center", "scale"),
      tuneLength = 5
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc); registerDoParallel(cl);
  g2 <- suppressWarnings(
    train(
      mufun_g,
      data = dat_g, 
      method = "glmnet", 
      trControl = tctrl,
      preProc = c("center", "scale"),
      tuneLength = 5
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc); registerDoParallel(cl);
  h2 <- suppressWarnings(
    train(
      mufun_h, 
      data = dat_h, 
      method = "glmnet", 
      trControl = tctrl,
      preProc = c("center", "scale"),
      tuneLength = 5
    )
  )
  stopCluster(cl)
  
  
  print("knn")
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  f3 <- suppressWarnings(
    train(
      mufun_f,
      data = dat_f, 
      method = "knn", 
      trControl = tctrl,
      tuneLength = 5
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  g3 <- suppressWarnings(
    train(
      mufun_g,
      data = dat_g, 
      method = "knn", 
      trControl = tctrl,
      tuneLength = 5
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  h3 <- suppressWarnings(
    train(
      mufun_h,
      data = dat_h, 
      method = "knn", 
      trControl = tctrl,
      tuneLength = 5
    )
  )
  stopCluster(cl)
  
  
  print("rf")
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  f4 <- suppressWarnings(
    train(
      mufun_f,
      data = dat_f,
      method = "rf",
      trControl = tctrl,
      tuneLength = 5,
      ntree = 1000
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  g4 <- suppressWarnings(
    train(
      mufun_g,
      data = dat_g,
      method = "rf",
      trControl = tctrl,
      tuneLength = 5,
      ntree = 1000
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  h4 <- suppressWarnings(
    train(
      mufun_h,
      data = dat_h,
      method = "rf",
      trControl = tctrl,
      tuneLength = 5,
      ntree = 1000
    )
  )
  stopCluster(cl)
  
  
  print("gbm")
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  f5 <- suppressWarnings(
    train(
      mufun_f,
      data = dat_f,
      method = "gbm",
      trControl = tctrl,
      tuneGrid = expand.grid( 
        # number of base learner
        n.trees = c(500, 1000),             
        # depth
        interaction.depth = c(10, 15, 20),  
        # learning rate
        shrinkage = c(0.005, 0.01, 0.02),   
        # minimum number of obs in the terminal node
        n.minobsinnode = 10                 
      ),
      verbose = FALSE
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  g5 <- suppressWarnings(
    train(
      mufun_g,
      data = dat_g,
      method = "gbm",
      trControl = tctrl,
      tuneGrid = expand.grid( 
        # number of base learner
        n.trees = c(500, 1000),             
        # depth
        interaction.depth = c(10, 15, 20),  
        # learning rate
        shrinkage = c(0.005, 0.01, 0.02),   
        # minimum number of obs in the terminal node
        n.minobsinnode = 10                 
      ),
      verbose = FALSE
    )
  )
  stopCluster(cl)
  
  set.seed(1)
  cl <- makeCluster(mc)
  registerDoParallel(cl)
  h5 <- suppressWarnings(
    train(
      mufun_h,
      data = dat_h,
      method = "gbm",
      trControl = tctrl,
      tuneGrid = expand.grid( 
        # number of base learner
        n.trees = c(500, 1000),             
        # depth
        interaction.depth = c(10, 15, 20),  
        # learning rate
        shrinkage = c(0.005, 0.01, 0.02),   
        # minimum number of obs in the terminal node
        n.minobsinnode = 10                 
      ),
      verbose = FALSE
    )
  )
  stopCluster(cl)
  
  
  print("clreg")
  f6 <- suppressWarnings(
    clreg(
      # target's name
      target = "y",  
      
      # input's name
      inputs = c(
        feature0
      ),
      
      # input's name for pc 
      pcomps = list(  
        pc1 = feature1,
        pc2 = feature2
      ),
      
      # threshold for pc
      thresh = list(       
        pc1 = 0.90,
        pc2 = 0.90
      ),
      
      # y-quantiles for cluster
      ycut = list(
        lwr = 0.10,
        upr = 0.90,
        int = 0.05
      ),
      
      # mean-formulas
      mufuns = list(
        lwr = clreg_lwr,
        upr = clreg_upr
      ),
      
      # dataset (training/validation)
      data = list(
        dat_tr = dataset[(dataset$year <= (max(dataset$year)-1)), , drop=F],
        dat_te = dataset[(dataset$year == (max(dataset$year)  )), , drop=F]
      ),
      
      # cv-ROC/cv-PR curves
      cvplot = F
    )
  )
  
  g6 <- suppressWarnings(
    clreg(
      # target's name
      target = "y",  
      
      # input's name
      inputs = c(
        feature0
      ),  
      
      # input's name for pc 
      pcomps = list(  
        pc1 = feature1,
        pc2 = feature2,
        pc3 = feature3
      ),
      
      # threshold for pc
      thresh = list(       
        pc1 = 0.90,
        pc2 = 0.90,
        pc3 = 0.90
      ),
      
      # y-quantiles for cluster
      ycut = list(
        lwr = 0.10,
        upr = 0.90,
        int = 0.05
      ),
      
      # mean-formulas
      mufuns = list(
        lwr = clreg_lwr,
        upr = clreg_upr
      ),
      
      # dataset (training/validation)
      data = list(
        dat_tr = dataset[(dataset$year <= (max(dataset$year)-1)), , drop=F],
        dat_te = dataset[(dataset$year == (max(dataset$year)  )), , drop=F]
      ),
      
      # cv-ROC/cv-PR curves
      cvplot = F
    )
  )
  
  h6 <- suppressWarnings(
    clreg(
      # target's name
      target = "y",  
      
      # input's name
      inputs = c(
        feature0
      ),   
      
      # input's name for pc 
      pcomps = list(  
        pc1 = feature1,
        pc2 = feature2,
        pc3 = feature3,
        pc4 = feature4
      ),
      
      # threshold for pc
      thresh = list(       
        pc1 = 0.90,
        pc2 = 0.90,
        pc3 = 0.90,
        pc4 = 0.90
      ),
      
      # y-quantiles for cluster
      ycut = list(
        lwr = 0.10,
        upr = 0.90,
        int = 0.05
      ),
      
      # mean-formulas
      mufuns = list(
        lwr = clreg_lwr,
        upr = clreg_upr
      ),
      
      # dataset (training/validation)
      data = list(
        dat_tr = dataset[(dataset$year <= (max(dataset$year)-1)), , drop=F],
        dat_te = dataset[(dataset$year == (max(dataset$year)  )), , drop=F]
      ),
      
      # cv-ROC/cv-PR curves
      cvplot = F
    )
  )
  
  
  # saving all objects
  list(
    feature = list(
      feature0 = feature0,
      feature1 = feature1,
      feature2 = feature2,
      feature3 = feature3,
      feature4 = feature4
    ),
    
    preProc = list(
      preProc1 = preProc1,
      preProc2 = preProc2,
      preProc3 = preProc3,
      preProc4 = preProc4
    ),
    
    lm = list(
      f1, g1, h1
    ),
    
    glmnet = list(
      f2, g2, h2
    ),
    
    knn = list(
      f3, g3, h3
    ),
    
    rf = list(
      f4, g4, h4
    ),
    
    gbm = list(
      f5, g5, h5
    ),
    
    clreg = list(
      f6, g6, h6
    )
  )
  
}



# prediction output code
pred_out <- function(
  object, 
  pdat
) {
  
  # loading feature and preProc objects
  feature0 <- object$feature$feature0
  
  feature1 <- object$feature$feature1
  feature2 <- object$feature$feature2
  feature3 <- object$feature$feature3
  feature4 <- object$feature$feature4
  
  preProc1 <- object$preProc$preProc1
  preProc2 <- object$preProc$preProc2
  preProc3 <- object$preProc$preProc3
  preProc4 <- object$preProc$preProc4
  
  # pc-augmented dataset
  pc1 <- predict(object=preProc1, newdata=pdat[,colnames(pdat)%in%feature1, drop=F])
  pc2 <- predict(object=preProc2, newdata=pdat[,colnames(pdat)%in%feature2, drop=F])
  pc3 <- predict(object=preProc3, newdata=pdat[,colnames(pdat)%in%feature3, drop=F])
  pc4 <- predict(object=preProc4, newdata=pdat[,colnames(pdat)%in%feature4, drop=F])
  colnames(pc1) <- paste0(colnames(pc1), "_1")
  colnames(pc2) <- paste0(colnames(pc2), "_2")
  colnames(pc3) <- paste0(colnames(pc3), "_3")
  colnames(pc4) <- paste0(colnames(pc4), "_4")
  
  pdat_0 <- pdat[, colnames(pdat) %in% c(
    "sgg_code", "region", "year", "beg_date", "end_date", 
    feature0
  ), drop = F]
  pdat_f <- cbind.data.frame(
    pdat_0,
    pc1,
    pc2
  )
  pdat_g <- cbind.data.frame(
    pdat_0,
    pc1,
    pc2,
    pc3
  )
  pdat_h <- cbind.data.frame(
    pdat_0,
    pc1,
    pc2,
    pc3,
    pc4
  )
  pdat_list <- list(
    pdat_f,
    pdat_g,
    pdat_h
  )
  
  pred_lm <- do.call(
    what = "cbind", 
    args = mapply(
      FUN = function(mod, dat) {
        exp( 
          predict(
            object = mod, 
            newdata = dat
          ) 
        )
      },
      object$lm,
      pdat_list, 
      SIMPLIFY = F
    )
  )
  colnames(pred_lm) <- sprintf("lm%d", 1:3)
  
  pred_glmnet <- do.call(
    what = "cbind", 
    args = mapply(
      FUN = function(mod, dat) {
        exp( 
          predict(
            object = mod, 
            newdata = dat
          ) 
        )
      },
      object$glmnet,
      pdat_list, 
      SIMPLIFY = F
    )
  )
  colnames(pred_glmnet) <- sprintf("glmnet%d", 1:3)
  
  pred_knn <- do.call(
    what = "cbind", 
    args = mapply(
      FUN = function(mod, dat) {
        exp( 
          predict(
            object = mod, 
            newdata = dat
          ) 
        )
      },
      object$knn,
      pdat_list, 
      SIMPLIFY = F
    )
  )
  colnames(pred_knn) <- sprintf("knn%d", 1:3)
  
  pred_rf <- do.call(
    what = "cbind", 
    args = mapply(
      FUN = function(mod, dat) {
        exp( 
          predict(
            object = mod, 
            newdata = dat
          ) 
        ) 
      },
      object$rf,
      pdat_list, 
      SIMPLIFY = F
    )
  )
  colnames(pred_rf) <- sprintf("rf%d", 1:3)
  
  pred_gbm <- do.call(
    what = "cbind", 
    args = mapply(
      FUN = function(mod, dat) {
        exp( 
          predict(
            object = mod, 
            newdata = dat
          ) 
        )
      },
      object$gbm,
      pdat_list, 
      SIMPLIFY = F
    )
  )
  colnames(pred_gbm) <- sprintf("gbm%d", 1:3)
  
  pred_clreg <- do.call(
    what = "cbind", 
    args = sapply(
      X = lapply(
        X = object$clreg,
        FUN = function(mod) {
          odat <- suppressWarnings(
            predict.clreg(
              object = mod, 
              newdata = pdat
            )
          )
          odat <- subset(
            x = odat, 
            select = c(
              "sgg_code", "year", "region", "beg_date", "end_date", "pred"
            )
          )
          o <- order(
            odat$region, 
            odat$sgg_code, 
            odat$year, 
            odat$beg_date, 
            odat$end_date
          )
          odat <- odat[o,]
          odat
        }
      ),
      FUN = function(x) {
        x$pred
      },  
      simplify = F
    )
  )
  colnames(pred_clreg) <- sprintf("clreg%d", 1:3)
  
  pred_ml <- cbind.data.frame(
    region = pdat$region,
    sgg_code = pdat$sgg_code,
    year = pdat$year,
    beg_date = pdat$beg_date,
    end_date = pdat$end_date,
    pred_lm,
    pred_glmnet,
    pred_knn,
    pred_rf,
    pred_gbm
  )
  o <- order(
    pred_ml$region, 
    pred_ml$sgg_code, 
    pred_ml$year, 
    pred_ml$beg_date, 
    pred_ml$end_date
  )
  pred_ml <- pred_ml[o,]
  
  pred <- cbind.data.frame(
    pred_ml,
    pred_clreg
  )
  
  pred
}



# validation output code
hval_out <- function(
  pout, 
  vdat
) {
  
  vout <- merge(
    x = pout, 
    y = subset(
      x = vdat, 
      select = c(
        "region", "sgg_code", "year", "beg_date", "end_date", "y"
      )
    ), 
    by = c(
      "region", "sgg_code", "year", "beg_date", "end_date"
    )
  )
  pred <- vout[,!colnames(vout) %in% c(
    "region", "sgg_code", "year", "beg_date", "end_date", "y"
  )]
  yact <- vout$y
  
  rmse <- apply(
    X = pred, 
    MARGIN = 2, 
    FUN = function(x) {
      caret::RMSE(pred = x, obs = yact)
    }
  )
  nrmse <- rmse / diff(range(yact))
    
  mape <- apply(
    X = pred, 
    MARGIN = 2, 
    FUN = function(x) {
      caret::R2(pred = x, obs = yact)
      mean(abs(yact - x)/yact)
    }
  )
  
  vres <- cbind.data.frame(
    ml = rep(c("lm", "glmnet", "knn", "rf", "gbm", "clreg"), each = 3),
    type = as.factor(rep(x = c(1,2,3), times = 6)),
    rmse = rmse,
    nrmse = nrmse,
    mape = mape
  )
  rownames(vres) <- NULL
  
  tt <- paste0(
    "Region:", pout$region, ", ",
    "The best: ", 
    vres$ml[which.min(vres$rmse)], 
    vres$type[which.min(vres$rmse)]
  )
  plt <- ggplot(data = vres, mapping = aes(fill = type, y = rmse, x = ml)) +
    geom_bar(position = "dodge", stat = "identity") + 
    ggtitle(tt) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(face="bold", color="#993333", size=14),
      axis.text.y = element_text(face="bold", color="#993333", size=14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    ) 
  
  return(
    list(
      vres = vres,
      vout = vout,
      plt = plt,
      best = paste0(
        vres$ml[which.min(vres$rmse)], 
        vres$type[which.min(vres$rmse)]
      )
    )
  )
}



# min-max based prediction intervals
pint_mm <- function(
  hlist
) {
  
  yact <- unlist(lapply(
    X = hlist, 
    FUN = function(x) x$vout$y
  ))
  pred <- do.call(
    what = "cbind", 
    args = lapply(X = hlist, FUN = function(x) { 
      t(x$vout[, colnames(x$vout) %in% paste0(x$vres$ml, x$vres$type)])
    })
  )
  pred_avg <- apply(
    X = pred, 
    MARGIN = 2, 
    FUN = mean
  )
  pred_list <- lapply(
    X = hlist, 
    FUN = function(x) { 
      t(x$vout[, colnames(x$vout) %in% paste0(x$vres$ml, x$vres$type)])
    }
  )
  vres_list <- lapply(
    X = hlist, 
    FUN = function(x) {
      cbind.data.frame(
        method = paste0(x$vres$ml, x$vres$type),
        x$vres[,c("rmse","nrmse","mape")]
      )
    }
  )
  
  
  pred_lwr <- apply(pred, 2, FUN = quantile, prob=0)
  pred_upr <- apply(pred, 2, FUN = quantile, prob=1)
  ncov <- sum((pred_lwr <= yact) & (yact <= pred_upr))
  pcov <- round(
    x = ncov / length(yact), 
    digits = 2
  )
  
  pred_dat <- cbind.data.frame(
    yobs = as.factor(rep(1:ncol(pred), each = nrow(pred))),
    yact = rep(yact, each = nrow(pred)),
    pred = c(pred),
    pavg = rep(pred_avg, each = nrow(pred)),
    lwr = rep(pred_lwr, each = nrow(pred)),
    upr = rep(pred_upr, each = nrow(pred))
  )
  
  
  # raw scale
  plot_raw <- ggplot(data = pred_dat, aes(x=yobs, y=pred, fill=yobs)) + 
    # geom_boxplot(outlier.shape = NA, outlier.size = 2) +
    ggtitle(
      label = paste0(
        "min ~ max: ", ncov, "/", length(yact), "(", pcov, ")"
      )
    ) +
    scale_x_discrete(limits=as.character(unique(pred_dat$yobs))) + 
    scale_fill_discrete(limits=as.character(unique(pred_dat$yobs)))+
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=5, angle=90),
          axis.text.y = element_text(face="bold", color="#993333", size=5),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11)) + 
    geom_point(
      aes(x = yobs, y = (yact)), pch=21, cex=2.5,
      show.legend = F
    )   +
    geom_point(
      aes(x = yobs, y = (pavg)), pch=1, cex=2.0,
      show.legend = F
    )   +
    geom_errorbar(aes(ymin = (lwr), ymax = (upr)), width=.3) +
    labs(x="Events", y="Predicted values")  
  
  
  # log scale
  plot_log <- ggplot(data = pred_dat, aes(x=yobs, y=log(pred), fill=yobs)) + 
    # geom_boxplot(outlier.shape = NA, outlier.size = 2) +
    ggtitle(
      label = paste0(
        "min ~ max: ", ncov, "/", length(yact), "(", pcov, ")"
      )
    ) +
    scale_x_discrete(limits=as.character(unique(pred_dat$yobs))) + 
    scale_fill_discrete(limits=as.character(unique(pred_dat$yobs)))+
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", color="#993333", size=5, angle=90),
          axis.text.y = element_text(face="bold", color="#993333", size=5),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11)) + 
    geom_point(
      aes(x = yobs, y = log(yact)), pch=21, cex=2.5,
      show.legend = F
    )   +
    geom_point(
      aes(x = yobs, y = log(pavg)), pch=1, cex=2.0,
      show.legend = F
    )   +
    geom_errorbar(aes(ymin = log(lwr), ymax = log(upr)), width=.5) +
    labs(x="Events", y="log(Predicted values)")  
  
  
  # savings
  return(
    list(
      yact = yact,
      pred = pred,
      pcov = pcov,
      pred_lwr = pred_lwr,
      pred_upr = pred_upr,
      plot_raw = plot_raw,
      plot_log = plot_log
    )
  )
}

# End..

