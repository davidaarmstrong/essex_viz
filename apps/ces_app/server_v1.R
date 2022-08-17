##TODO Make p1 figure size responsive to environment. 


library(shiny)
library(htmltools)
library(plotly)
library(ggmosaic)
library(effects)
library(haven)
library(ggplot2)
library(plotly)
library(DAMisc)
library(dplyr)
library(tidyr)
library(mitools)
library(htmlTable)
library(DT)
library(janitor)
library(survey)
library(factorplot)
load("ces0415imp.rda")


#    Add to your server
# observeEvent(input$browser,{
#     browser()
# })


dv.names <- c("Vote Liberal"  = "vote_lib", "Vote Conservative" = "vote_con", 
              "Vote NDP" = "vote_ndp", "Vote BQ" = "vote_bloc")
blocks <- list()
blocks[[1]] <- c("gender", "agegrp", "cathol", "educ", "union", "year_fac", "province")
blocks[[2]] <- c("doquebec", "continent", "market", "moral", "cynicism", "alienreg")
blocks[[3]] <- c("pid", "pidng", "pidnb", "pidnbg")
blocks[[4]] <- c("retroper", "retrocan")
blocks[[5]] <- c("taxpers", "taxcorp", "sp_health", "sp_defence", "sp_envir", "immig")
blocks[[6]] <- c("leader_con", "leader_lib", "leader_ndp", "leader_bloc")

nblocks <- list()
nblocks[[1]] <- c("Gender", "Age Group", "Catholic", "Schooling", "Union Household", "Year", "Region")
nblocks[[2]] <- c("Do More Quebec", "Continentalism", "Market Liberalism", "Moral Tradition", "Political Cynicism", "Regional Alienation")
nblocks[[3]] <- c("Party ID", "Party ID (No Green)", "Party ID (No BQ)", "Party ID (No Green or BQ)")
nblocks[[4]] <- c("Personal Retrospective", "National Retrospective")
nblocks[[5]] <- c("Personal Tax", "Corporate Tax", "Health Spend", "Defense Spend", "Environment Spend", "More Immigration")
nblocks[[6]] <- c("Feelings: Conservative", "Feelings: Liberal", "Feelings: NDP", "Feelings: BQ")

chc <- do.call("c", blocks)
names(chc) <- do.call("c", nblocks)

val_num_vars <- c("market", "moral", "cynicism", "continent", "leader_bloc", "leader_lib", "leader_ndp", "leader_con")
val_num_names <- c("Market Liberalism", "Moral Traditionalism", "Political Cynicism", "Continentalism", 
                   "Feeling Thermometer: BQ Leader", "Feeling Thermometer: Liberal Leader", 
                   "Feeling Thermometer: NDP Leader", "Feeling Thermometer: Conservative")

val_strat_vars <- c("none", "gender", "agegrp", "cathol", "educ", "union", "year", "province", "doquebec", "alienreg", "pid", "retroper", 
                    "retrocan", "taxpers", "taxcorp", "sp_health", "sp_defence", "sp_envir", "immig", "vote")
val_strat_names <- c("None", "Gender", "Age Group", "Catholic", "Schooling", "Union Household", "Year", "Region", 
                     "Do More Quebec", "Regional Anlienation", "Party ID", "Personal Retrospective Economy", 
                     "National Retrospective Economy", "Personal Tax", "Corporate Tax", "health Spending", 
                     "Defense Spending", "Environment Spending", "More Immigration?", "Vote")

strat_chc <- val_strat_vars
names(strat_chc) <- val_strat_names


des <- svydesign(ids=~1, strata=NULL, weights=~weight, data=newdat, digits=3)
DES <- lapply(1:length(ndlist), function(i){svydesign(ids=~1, strata=NULL, weights=~weight, data=ndlist[[i]], digits=3)})

pre <-
  function (mod1, mod2 = NULL, design, sim = FALSE, R = 2500){
    if (is.null(mod2)) {
      y <- mod1[["y"]]
      mod2 <- update(mod1, ". ~ 1", design=design)
    }
    pred.mod2 <- as.numeric(predict(mod2, type = "response") >=
                              0.5)
    pmc <- mean(mod2$y == pred.mod2)
    pred.y <- as.numeric(predict(mod1, type = "response") >=
                           0.5)
    pcp <- mean(pred.y == mod1$y)
    pre <- (pcp - pmc)/(1 - pmc)
    pred.prob1 <- predict(mod1, type = "response")
    pred.prob2 <- predict(mod2, type = "response")
    epcp <- (1/length(pred.prob1)) * (sum(pred.prob1[which(mod1$y ==
                                                             1)]) + sum(1 - pred.prob1[which(mod1$y == 0)]))
    epmc <- (1/length(pred.prob2)) * (sum(pred.prob2[which(mod2$y ==
                                                             1)]) + sum(1 - pred.prob2[which(mod2$y == 0)]))
    epre <- (epcp - epmc)/(1 - epmc)
    if (sim) {
      b1.sim <- mvrnorm(R, coef(mod1), vcov(mod1))
      b2.sim <- mvrnorm(R, coef(mod2), vcov(mod2))
      mod1.probs <- family(mod1)$linkinv(model.matrix(mod1) %*%
                                           t(b1.sim))
      mod2.probs <- family(mod2)$linkinv(model.matrix(mod2) %*%
                                           t(b2.sim))
      pmcs <- apply(mod2.probs, 2, function(x) mean(as.numeric(x >
                                                                 0.5) == mod2$y))
      pcps <- apply(mod1.probs, 2, function(x) mean(as.numeric(x >
                                                                 0.5) == mod1$y))
      pre.sim <- (pcps - pmcs)/(1 - pmcs)
      epmc.sim <- apply(mod2.probs, 2, function(x) (1/length(x)) *
                          (sum(x[which(mod2$y == 1)]) + sum(1 - x[which(mod2$y ==
                                                                          0)])))
      epcp.sim <- apply(mod1.probs, 2, function(x) (1/length(x)) *
                          (sum(x[which(mod1$y == 1)]) + sum(1 - x[which(mod1$y ==
                                                                          0)])))
      epre.sim <- (epcp.sim - epmc.sim)/(1 - epmc.sim)
    }
      ret <- list()
      ret$pre <- pre
      ret$epre <- epre
      form1 <- formula(mod1)
      form2 <- formula(mod2)
      ret$m1form <- paste(form1[2], form1[1], form1[3], sep = " ")
      ret$m2form <- paste(form2[2], form2[1], form2[3], sep = " ")
      ret$pcp <- pcp
      ret$pmc <- pmc
      ret$epmc <- epmc
      ret$epcp <- epcp
      if (sim) {
        ret$pre.sim <- pre.sim
        ret$epre.sim <- epre.sim
      }
      class(ret) <- "pre"
      return(ret)
}
    
sumStats <- function(d, vars, byvar=NULL, convertFactors=FALSE){
  if(is.null(byvar)){
    out <- vector(mode="list", length=1)
    forms <- lapply(vars, function(x)as.formula(paste0("~", x)))
    means <- sapply(forms, function(x)as.vector(svymean(x, d, na.rm=TRUE)))
    sds <- sapply(forms, function(x)sqrt(svyvar(x, d, na.rm=TRUE)[1]))
    qtiles <- t(sapply(forms, function(x)svyquantile(x, d, quantiles=c(0,.25,.5,.75,1), na.rm=TRUE)))
    iqr <- qtiles[,4]-qtiles[,2]
    obs.mat <- as.matrix(!is.na(as.matrix(d$variables[,vars])))
    obs.mat <- apply(obs.mat, 2, as.numeric)
    wtvec <- as.numeric(as.vector(d$variables$weight))
    n <- ceiling(c(wtvec %*% obs.mat))
    na <- sum(d$variables$weight) - n
    tmpdf <- data.frame(group = "All Observations", variable= val_num_names[match(vars, val_num_vars)])
    out[[1]] <- as.data.frame(cbind(means, sds, iqr, qtiles, n, na))
    names(out[[1]]) <- c("Mean", "SD", "IQR", "0%", "25%", "50%", "75%", "100%", "n", "NA")
    out[[1]] <- cbind(tmpdf, out[[1]])
    rownames(out[[1]]) <- NULL
  }
  else{
    if(!is.factor(d$variables[[byvar]])){
      d$variables[[byvar]] <- as.factor(d$variables[[byvar]])
    }
    out <- vector(mode="list", length=length(vars))
    forms <- lapply(vars, function(x)as.formula(paste0("~", x)))
    byform <- as.formula(paste0("~", byvar))
    means <- lapply(forms, function(x)svyby(x, byform, d, svymean, na.rm=TRUE))
    sds <- lapply(forms, function(x)svyby(x, byform, d, svyvar, na.rm=TRUE))
    qtiles <- lapply(forms, function(x)svyby(x, byform, d, svyquantile, 
                      quantiles=c(0,.25,.5,.75,1), na.rm=TRUE, keep.var=FALSE))
    iqr <- lapply(qtiles, function(x)x[,4]-x[,2])
    n <- lapply(vars, function(x)svytable(byform, 
                        subset(d, !is.na(eval(parse(text=x))))))
    n <- lapply(n, function(x){names(x) <- NULL; c(x)})
    allobs <- svytable(byform, d)
    na <- lapply(n, function(x)allobs - x)
    na <- lapply(na, function(x){names(x) <- NULL; c(x)})
    for(i in 1:length(out)){
      out[[i]] <- cbind(means[[i]][,2], sds[[i]][,2], iqr[[i]], qtiles[[i]][,-1], n[[i]][], na[[i]][])
      colnames(out[[i]]) <- c("Mean", "SD", "IQR", "0%", "25%", "50%", 
                              "75%", "100%", "n", "NA")
      tmpdf <- data.frame(group = factor(1:length(rownames(means[[1]])), labels=rownames(means[[1]])), 
                          variable= val_num_names[match(vars[i], val_num_vars)])
      out[[i]] <- cbind(tmpdf, as.data.frame(out[[i]]))
      rownames(out[[i]]) <- NULL
    }
    }
  out <- do.call(rbind, out)
  out
}

sumFacs <- function(d, var, byvar=NULL){
  if(is.null(byvar)){
    tab <- svytable(as.formula(paste0("~", var)), d)
    tab <- tab %>% as.data.frame() %>% 
      adorn_totals(c("row")) %>%
      adorn_percentages("col") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 0) %>%
      adorn_ns() 
      } else{
    tab <- svytable(as.formula(paste0("~", var, "+", byvar)), d)
    chi2 <- svychisq(as.formula(paste0("~", var, "+", byvar)), d)
    tab <- tab %>% 
            as_tibble() %>% 
            pivot_wider(names_from=byvar, values_from = n) %>% 
            as.data.frame  
            attr(tab, "var_names") <- list(row = var, col = byvar)
            tab <- tab %>% 
            adorn_totals(c("row", "col")) %>%
            adorn_percentages("col") %>% 
            adorn_pct_formatting(rounding = "half up", digits = 0) %>%
            adorn_ns() %>%
            adorn_title("combined") 
  } 
  
  tab
}

X2fun <- function(d, var, byvar=NULL){
  tab <- svytable(as.formula(paste0("~", var, "+", byvar)), d)
  chi2 <- svychisq(as.formula(paste0("~", var, "+", byvar)), d)
  chi2
}

probci2 <- function(obj, data, .b=NULL, .vcov=NULL, changeX=NULL, numQuantVals=5, xvals = NULL, type=c("aveEff", "aveCase")){
  type <- match.arg(type)
  vn <- changeX
  if(length(vn) == 0){stop("Need at least one variable to change")}
  vals <- vector(length=length(vn), mode="list")
  names(vals) <- vn
  for(i in 1:length(vn)){
    if(is.factor(data[[vn[i]]])){
      vals[[i]] <- factor(1:length(levels(data[[vn[i]]])), labels=levels(data[[vn[i]]]))
    }
    if(!is.null(xvals[[vn[i]]])){
      vals[[i]] <- xvals[[vn[i]]]
    }
    else{
      if(!is.factor(data[[vn[i]]]) & length(unique(na.omit(data[[vn[i]]]))) <= numQuantVals){
        vals[[i]] <- sort(unique(na.omit(data[[vn[i]]])))
      }
      if(!is.factor(data[[vn[i]]]) & length(unique(na.omit(data[[vn[i]]]))) > numQuantVals){
        vals[[i]] <- seq(min(data[[vn[i]]], na.rm=TRUE), max(data[[vn[i]]], na.rm=TRUE), length = numQuantVals)
      }
    }
  }
  egvals <- do.call(expand.grid, vals)
  if(is.null(.b)){
    b <- coef(obj)
  }
  else{
    b <- .b
  }
  if(is.null(.vcov)){
    v <- vcov(obj)
  }
  else{
    v <- .vcov
  }
  require(MASS)
  bmat <- t(mvrnorm(2500, b, v, empirical=TRUE))
  if(type == "aveCase"){
    av <- all.vars(obj$formula)
    others <- av[-which(av %in% vn)]
    othervals <- lapply(1:length(others), function(i)central(data[[others[i]]]))
    names(othervals) <- others
    otherdat <- do.call(data.frame, othervals)
    alldat <- do.call(expand.grid, c(vals, otherdat))
    X <- model.matrix(formula(obj), data=alldat)
    probs <- t(family(obj)$linkinv(X %*% bmat))
    probci <- t(apply(probs, 2, quantile, c(.5,.025,.975)))[,,drop=FALSE]
    ev <- sapply(1:ncol(egvals), function(i)paste(colnames(egvals)[i], "=", egvals[,i], sep=""))[,,drop=FALSE]
    probn <- apply(ev, 1, paste, collapse=", ")
    rownames(probci) <- probn
    probci <- as.data.frame(probci)
  }
  if(type == "aveEff"){
    probs <- vector(mode="list", length=nrow(egvals))
    for(i in 1:nrow(egvals)){
      tmp <- data
      for(j in 1:ncol(egvals)){
        tmp[, names(egvals)[j]] <- egvals[i,j]
      }
      X <- model.matrix(formula(obj), data=tmp)
      probs[[i]] <- family(obj)$linkinv(X %*% bmat)
    }
    probci <- t(apply(sapply(probs, colMeans), 2, quantile, c(.5,.025, .975)))[,,drop=FALSE]
    ev <- sapply(1:ncol(egvals), function(i)paste(colnames(egvals)[i], "=", egvals[,i], sep=""))[,,drop=FALSE]
    probn <- apply(ev, 1, paste, collapse=", ")
    rownames(probci) <- probn
    probci <- as.data.frame(probci)
  }
  res <- list("Predicted Probabilities"=probci, plot.data = cbind(egvals, probci))
  return(res)
}
central <- function(x){
  if(is.factor(x)){
    tab <- table(x)
    m <- which.max(tab)
    cent <- factor(m, levels=1:length(levels(x)), labels=levels(x))
  }
  if(!is.factor(x) & length(unique(na.omit(x))) <= 10){
    tab <- table(x)
    cent <- as.numeric(names(tab)[which.max(tab)])
  }
  if(!is.factor(x) & length(unique(na.omit(x))) > 10){
    cent <- median(x, na.rm=TRUE)
  }
  return(cent)
}

blocks <- list()
blocks[[1]] <- c("gender", "agegrp", "cathol", "educ", "union", "year_fac", "province")
blocks[[2]] <- c("doquebec", "continent", "market", "moral", "cynicism", "alienreg")
blocks[[3]] <- c("pid", "pidng", "pidnb", "pidnbg")
blocks[[4]] <- c("retroper", "retrocan")
blocks[[5]] <- c("taxpers", "taxcorp", "sp_health", "sp_defence", "sp_envir", "immig")
blocks[[6]] <- c("leader_con", "leader_lib", "leader_ndp", "leader_bloc")

nblocks <- list()
nblocks[[1]] <- c("Gender", "Age Group", "Catholic", "Schooling", "Union Household", "Year", "Region")
nblocks[[2]] <- c("Do More Quebec", "Continentalism", "Market Liberalism", "Moral Tradition", "Political Cynicism", "Regional Alienation")
nblocks[[3]] <- c("Party ID", "Party ID (No Green)", "Party ID (No BQ)", "Party ID (No Green or BQ)")
nblocks[[4]] <- c("Personal Retrospective", "National Retrospective")
nblocks[[5]] <- c("Personal Tax", "Corporate Tax", "Health Spend", "Defense Spend", "Environment Spend", "More Immigration")
nblocks[[6]] <- c("Feelings: Conservative", "Feelings: Liberal", "Feelings: NDP", "Feelings: BQ")
b <- do.call("c", blocks)
n <- do.call("c", nblocks)


val_num_vars <- c("market", "moral", "cynicism", "continent", "leader_bloc", "leader_lib", "leader_ndp", "leader_con")
val_num_names <- c("Market Liberalism", "Moral Traditionalism", "Political Cynicism", "Continentalism", 
                   "Feeling Thermometer: BQ Leader", "Feeling Thermometer: Liberal Leader", 
                   "Feeling Thermometer: NDP Leader", "Feeling Thermometer: Conservative")

val_strat_vars <- c("none", "gender", "agegrp", "cathol", "educ", "union", "year", "province", "doquebec", "alienreg", "pid", "retroper", 
                    "retrocan", "taxpers", "taxcorp", "sp_health", "sp_defence", "sp_envir", "immig", "vote")
val_strat_names <- c("None", "Gender", "Age Group", "Catholic", "Schooling", "Union Household", "Year", "Region", 
                     "Do More Quebec", "Regional Anlienation", "Party ID", "Personal Retrospective Economy", 
                     "National Retrospective Economy", "Personal Tax", "Corporate Tax", "health Spending", 
                     "Defense Spending", "Environment Spending", "More Immigration?", "Vote")

strat_chc <- val_strat_vars
names(strat_chc) <- val_strat_names



shinyServer <- function(input, output, session) {
  tmpdat <- reactive({
    x <- list()
    for(i in 1:5){
      x[[i]] <- subset(DES[[i]], province %in% input$provs & year %in% input$years)
      if(input$impdv == "no"){
        x[[i]] <- subset(x[[i]], !is.na(orig_vote))
      }
      for(j in 1:ncol(x[[i]]$variables)){
        if(is.factor(x[[i]]$variables[[j]])){
          x[[i]]$variables[[j]] <- droplevels(x[[i]]$variables[[j]])
        }
        if(is.labelled(x[[i]]$variables[[j]])){ x[[i]]$variables[[j]] <- as.numeric(x[[i]]$variables[[j]])}
      }
      }
    x
  })
    m1 <- reactive({
    req(input$mod1)  
    iv1 <- input$mod1
    if(input$varby == "year"){
      iv1 <- c(iv1, paste0("year_fac*", input$focus_var))
    }
    if(input$varby == "region"){
      iv1 <- c(iv1, paste0("province*", input$focus_var))
    }
    if(length(input$years) == 1 & "year_fac" %in% iv1){
      iv1 <- iv1[-grep("year_fac", iv1)]
    }
    if(length(input$provs) == 1 & "province" %in% iv1){
      iv1 <- iv1[-grep("province", iv1)]
    }
    if(length(iv1) > 0){
        form1 <- formula(paste0(input$dv, "~", paste(iv1, collapse=" + ")))
        glm(form1, data=tmpdat()[[1]]$variables, family=binomial)
     }
    })
    
    m1a <- reactive({
      req(input$mod1)
      iv1 <- input$mod1
      if(input$varby == "year"){
        iv1 <- c(iv1, paste0("year_fac*", input$focus_var))
      }
      if(input$varby == "region"){
        iv1 <- c(iv1, paste0("province*", input$focus_var))
      }
      if(length(input$years) == 1 & "year_fac" %in% iv1){
        iv1 <- iv1[-which(iv1 == "year_fac")]
      }
      if(length(input$provs) == 1 & "province" %in% iv1){
        iv1 <- iv1[-which(iv1 == "province")]
      }
      if(length(iv1) > 0){
        form1 <- formula(paste0(input$dv, "~", paste(iv1, collapse=" + ")))
        xl <- f1 <- list()
        for(i in 1:5){
            xl[[i]] <- svyglm(form1, design=tmpdat()[[i]], family=binomial)
            f1[[i]] <- pre(xl[[i]], design=tmpdat()[[i]])
          }
          x <- MIcombine(xl)
          list(res=x, pre=rowMeans(sapply(f1, function(x)c(unlist(x[c("pmc", "pcp", "pre", "epmc", "epcp", "epre")])))))      }
    })
    
    m2 <- reactive({
    req(input$mod2)
    iv2 <- input$mod2
    if(input$varby == "year"){
      iv2 <- c(iv2, paste0("year_fac*", input$focus_var))
    }
    if(input$varby == "region"){
      iv2 <- c(iv2, paste0("province*", input$focus_var))
    }
    if(length(input$years) == 1 & "year_fac" %in% iv2){
      iv2 <- iv2[-which(iv2 == "year_fac")]
    }
    if(length(input$provs) == 1 & "province" %in% iv2){
      iv2 <- iv2[-which(iv2 == "province")]
    }
    if(length(iv2) > 0){
      form2 <- formula(paste0(input$dv, "~", paste(iv2, collapse=" + ")))
      glm(form2, data=tmpdat()[[1]]$variables, family=binomial)
     }
  })
    
    m2a <- reactive({
      req(input$mod2)
      iv2 <- input$mod2
      if(input$varby == "year"){
        iv2 <- c(iv2, paste0("year_fac*", input$focus_var))
      }
      if(input$varby == "region"){
        iv2 <- c(iv2, paste0("province*", input$focus_var))
      }
      if(length(input$years) == 1 & "year_fac" %in% iv2){
        iv2 <- iv2[-which(iv2 == "year_fac")]
      }
      if(length(input$provs) == 1 & "province" %in% iv2){
        iv2 <- iv2[-which(iv2 == "province")]
      }
      if(length(iv2) > 0){
        form2 <- formula(paste0(input$dv, "~", paste(iv2, collapse=" + ")))
        xl <- f2 <- list()
        for(i in 1:5){
          xl[[i]] <- svyglm(form2, design=tmpdat()[[i]], family=binomial)
          f2[[i]] <- pre(xl[[i]], design=tmpdat()[[i]])
        }
        x <- MIcombine(xl)
        list(res=x, pre=rowMeans(sapply(f2, function(x)c(unlist(x[c("pmc", "pcp", "pre", "epmc", "epcp", "epre")])))))
      }
    })
    
  
  c1 <- reactive({
    req(m1())
    cx <- list("none" = input$focus_var, "year" = c("year_fac", input$focus_var), "region" = c("province", input$focus_var))
    out <- probci(m1(), tmpdat()[[1]]$variables, changeX=cx[[input$varby]], .b=m1a()$res$coefficients, .vcov=m1a()$res$variance, numQuantVals=2, type=input$type) 
    # if("year1" %in% names(out[[2]])){
    #   out[[2]]$year1 <- as.character(out[[2]]$year1)
    #   out[[2]]$year2 <- as.character(out[[2]]$year2)
    # }
    out      
  })
  c2 <- reactive({
    req(m2())
    cx <- list("none" = input$focus_var, "year" = c("year_fac", input$focus_var), "region" = c("province", input$focus_var))
    out <- probci(m2(), tmpdat()[[1]]$variables, changeX=cx[[input$varby]], .b=m2a()$res$coefficients, .vcov=m2a()$res$variance, numQuantVals=2, type=input$type) 
    out
  })
  c1a <- reactive({
    req(m1())
    cx <- list("none" = input$focus_var, "year" = c("year_fac", input$focus_var), "region" = c("province", input$focus_var))
    out <- probci2(m1(), tmpdat()[[1]]$variables, changeX=cx[[input$varby]], .b=m1a()$res$coefficients, .vcov=m1a()$res$variance, numQuantVals=2, type=input$type) 
    out
    })
  c2a <- reactive({
    req(m2())
    cx <- list("none" = input$focus_var, "year" = c("year_fac", input$focus_var), "region" = c("province", input$focus_var))
    out <- probci2(m2(), tmpdat()[[1]]$variables, changeX=cx[[input$varby]], .b=m2a()$res$coefficients, .vcov=m2a()$res$variance, numQuantVals=2, type=input$type) 
    out
  })
  d1 <- reactive({
    req(c1a())
    x <- c1a()$plot.data
    x$model <- "Model 1"
    rownames(x) <- NULL
    if(input$varby == "none"){
      names(x)[1:4] <- c("x", "fit", "lower", "upper")
    }
    if(input$varby == "year"){
      names(x)[1:5] <- c("year", "x", "fit", "lower", "upper")
    }
    if(input$varby == "region"){
      names(x)[1:5] <- c("region", "x", "fit", "lower", "upper")
    }
    x    
    
    })
  d2 <- reactive({
    req(c2a())
    x <- c2a()$plot.data
    x$model <- "Model 2"
    rownames(x) <- NULL
    if(input$varby == "none"){
      names(x)[1:4] <- c("x", "fit", "lower", "upper")
    }
    if(input$varby == "year"){
      names(x)[1:5] <- c("year", "x", "fit", "lower", "upper")
    }
    if(input$varby == "region"){
      names(x)[1:5] <- c("region", "x", "fit", "lower", "upper")
    }
    x    
  })
  
pd <- reactive({
  l <- list()
  i <- 1
  if(length(input$mod1) > 0){
    l[[i]] <- d1()
    i <- i+1
  }
  if(length(input$mod2) > 0){
    l[[i]] <- d2()
  }
  out <- do.call("rbind", l)
  names(out)[which(names(out) == "x")] <- "z"
  out
  })
  
  colvec <- reactive({
    colvec <- NULL
    if(length(input$mod1) >0){
      colvec <- c(colvec, input$m1col)
    }
    if(length(input$mod2) >0){
      colvec <- c(colvec, input$m2col)
    }
    colvec
  })


  output$p1 <- renderPlotly({
  if(isTruthy(input$focus_var) & isTruthy(colvec())){
    party <- switch(input$dv, 
                              "vote_lib" = "Liberal", 
                              "vote_con" = "Conservative", 
                              "vote_ndp" = "NDP", 
                              "vote_bq" = "Bloc Quebecois", 
                              "vote_grn" = "Green")
    
    if(is.factor(pd()$z)){
      if(input$varby == "none"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model)) + 
        geom_point(position=position_dodge(width=.5), size=3) + 
        geom_errorbar(width=0, position=position_dodge(width=.5), size=1.5) + 
        scale_colour_manual(values=colvec()) + 
        xlab(n[which(b == input$focus_var)]) +
        ylab(paste0("Pr(Vote ", party, ")")) +
        theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw()     
      }
      if(input$varby == "year"){
          g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model)) + 
          geom_point(position=position_dodge(width=.5), size=3) + 
          geom_errorbar(width=0, position=position_dodge(width=.5), size=1.5) + 
          scale_colour_manual(values=colvec()) + 
          xlab(n[which(b == input$focus_var)]) +
          ylab(paste0("Pr(Vote ", party, ")")) +
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~year)
       }
      if(input$varby == "region"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model)) + 
          geom_point(position=position_dodge(width=.5), size=3) + 
          geom_errorbar(width=0, position=position_dodge(width=.5), size=1.5) + 
          scale_colour_manual(values=colvec()) + 
          xlab(n[which(b == input$focus_var)]) +
          ylab(paste0("Pr(Vote ", party, ")")) +
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~region)
        }
    }
    else{
      if(input$varby == "none"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model, fill=model)) + 
          geom_ribbon(alpha=.25, linetype="blank") + 
          geom_line(size=1) + 
          scale_colour_manual(values=colvec()) + 
          scale_fill_manual(values=colvec()) + 
          xlab(n[which(b == input$focus_var)]) +
          ylab(paste0("Pr(Vote ", party, ")")) +
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw()
      }
      if(input$varby == "year"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model, fill=model)) + 
          geom_ribbon(alpha=.25, linetype="blank") + 
          geom_line(size=1) + 
          scale_colour_manual(values=colvec()) + 
          scale_fill_manual(values=colvec()) + 
          xlab(n[which(b == input$focus_var)]) +
          ylab(paste0("Pr(Vote ", party, ")")) +
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~year)
      }
      if(input$varby == "region"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model, fill=model)) + 
          geom_ribbon(alpha=.25, linetype="blank") + 
          geom_line(size=1) + 
          scale_colour_manual(values=colvec()) + 
          scale_fill_manual(values=colvec()) + 
          xlab(n[which(b == input$focus_var)]) +
          ylab(paste0("Pr(Vote ", party, ")")) +
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~region)
      }
   }
    ggplotly(g, height=450, width=450) %>% layout(autosize=TRUE, 
        xaxis=list(automargin=TRUE), yaxis=list(automargin=TRUE), 
        legend=list(orientation="h", x=input$xpos, y=input$ypos))
    }
  })
  
    output$t1a <- renderDataTable({
      req(c1())
      out <- c1()$plot.data
      DT::datatable(out, rownames=FALSE, filter="none", 
                options=list("paging" = FALSE, "searching"=FALSE, 
                             "info" = FALSE)) %>% formatRound(columns=c("pred_prob", "lower", "upper"), digits=3)
    })
    
    outx <- reactive({
      req(c1())
      tmp <- c1()$`Difference in Predicted Probabilities`        
      tmp
    })
    
    output$t1b <- DT::renderDataTable(
      datatable(outx(), 
      options=list("paging" = TRUE,
                   "info" = FALSE),
      filter="top") %>% formatRound(columns=c("pred_prob", "lower", "upper"), digits=3), 
      rownames=FALSE
      ) 

    output$t2a <- renderDataTable({
      req(c2())
      out <- c2()$plot.data
      DT::datatable(out, rownames=FALSE, filter="none", 
                    options=list("paging" = FALSE, "searching"=FALSE, 
                                 "info" = FALSE)) %>% formatRound(columns=c("pred_prob", "lower", "upper"), digits=3)
    })
    
    outx2 <- reactive({
      req(c2())
      tmp <- c2()$`Difference in Predicted Probabilities`        
      tmp
    })
    output$t2b <- DT::renderDataTable(
      datatable(outx2(), 
                options=list("paging" = TRUE,
                             "info" = FALSE),
                filter="top") %>% formatRound(columns=c("pred_prob", "lower", "upper"), digits=3), 
      rownames=FALSE
    ) 
    
    output$mo1 <- renderUI({
      s <- NULL
      if(length(input$mod1) > 0 & length(input$mod2) == 0){
        s <- stargazer(m1(), notes.append=FALSE,
                      star.cutoffs = .05,
                      notes="*p < 0.05, two-sided",
                      coef=list(coef(m1a()$res)),  
                      se = list(sqrt(diag(vcov(m1a()$res)))),
                      type="html", keep.stat="n")
        }
      if(length(input$mod1) == 0 & length(input$mod2) > 0){
        s <- stargazer(m2(), notes.append=FALSE,
                       star.cutoffs = .05,
                       notes="*p < 0.05, two-sided", 
                       coef=list(coef(m2a()$res)),  
                       se = list(sqrt(diag(vcov(m2a()$res)))),
                       type="html", keep.stat="n")
      }
      if(length(input$mod1) > 0 & length(input$mod2) > 0){
        s <- stargazer(m1(), m2(), notes.append=FALSE,
                       star.cutoffs = .05,
                       notes="*p < 0.05, two-sided",
                       coef=list(coef(m1a()$res), coef(m2a()$res)),  
                       se = list(sqrt(diag(vcov(m1a()$res))), sqrt(diag(vcov(m2a()$res)))),
                       type="html", keep.stat="n")
      }
      if(!is.null(s)){
        HTML(s)
      }
      })
    
    output$mfs <- renderUI({
      req(m1())
      div(br(),
      p("Model Fit Statistics"))

    })
    
    output$mo2 <- renderUI({
      tab <- NULL
      if(length(input$mod1) > 0 & length(input$mod2) == 0){
        t1 <- sprintf("%.3f", m1a()$pre)
        tab <- matrix(t1, ncol=1)
        rownames(tab) <- c("Prop. Correct (Null)", "Prop. Correct (Model)", "PRE", "Expected Prop. Correct (Null)", "Expected Prop. Correct (Model)", "Expected PRE")
        colnames(tab) <- "Model 1"        
      }
      if(length(input$mod1) == 0 & length(input$mod2) > 0){
        t2 <- sprintf("%.3f", m2a()$pre)
        tab <- matrix(t2, ncol=1)
        rownames(tab) <- c("Prop. Correct (Null)", "Prop. Correct (Model)", "PRE", "Expected Prop. Correct (Null)", "Expected Prop. Correct (Model)", "Expected PRE")
        colnames(tab) <- "Model 2"        
      }
      if(length(input$mod1) > 0 & length(input$mod2) > 0){
        t1 <- sprintf("%.3f", m1a()$pre)
        t2 <- sprintf("%.3f", m2a()$pre)
        tab <- cbind(t1, t2)
        rownames(tab) <- c("Prop. Correct (Null)", "Prop. Correct (Model)", "PRE", "Expected Prop. Correct (Null)", "Expected Prop. Correct (Model)", "Expected PRE")
        colnames(tab) <- c("Model 1", "Model 2")
      }
      if(!is.null(tab)){
        HTML(htmlTable(tab))
      }
    })
    
    sHist <- reactive({
      if(input$desvar1 != "none" & input$stratdes == "none"){
      out <- svyhist(as.formula(paste0("~", input$desvar1)), des)
      out <- as.list(out)
      out
      }
    })
    
    calcDescriptives <- reactive({
      if(input$desvar1 != "none"){
        if(input$stratdes != "none"){
          numOut <- sumStats(des, input$desvar1, byvar=input$stratdes)
        } 
        else{
          numOut <- sumStats(des, input$desvar1)    
        }
        rownames(numOut) <- NULL
        numOut      
      }
    })
    
    
    output$Descriptive <- renderDataTable({
      req(calcDescriptives())  
      out <- datatable(calcDescriptives(), rownames=FALSE, 
                         options=list("paging" = FALSE, "searching"=FALSE, "info"=FALSE)) %>% 
                         formatRound(columns=c("Mean", "SD", "IQR", "0%", "25%", "50%", "75%", "100%"), digits=3)
      out
      })    
    
    output$histPlot <- renderPlotly({
      req(sHist())
    })
    
    output$binInput <- renderUI({
      if(input$desvar1 != "none" & input$stratdes == "none"){
      numericInput("nbins", "Number of Bins", 10, 3, 100, step=1)
      }
    })
    
    output$descPlot <- renderPlotly({
      if(isTruthy(calcDescriptives()) | isTruthy(sHist())){
        if(nrow(calcDescriptives()) > 1){
          tmp <- calcDescriptives()
          tmp$Error <- 1.96*(tmp$SD/sqrt(tmp$n))
          tmp$text =paste('Mean:', sprintf("%.2f", tmp$Mean),
                         '<br>lower CI: ', sprintf("%.2f", tmp$Mean-tmp$Error),
                         '<br>upper CI: ', sprintf("%.2f", tmp$Mean+tmp$Error))
  
  
          g <- ggplot(tmp, aes(x=group, y=Mean, 
                               ymin=Mean - Error, ymax=Mean + Error,
                               text=text)) + 
            geom_point() + 
            geom_errorbar(width=0) + 
            theme_bw() + 
            labs(x="", y=paste0("Average ", as.character(tmp$variable[1]))) 
          o <- ggplotly(g, tooltip="text") 
        }
        if(isTruthy(input$nbins)){
          if(input$desvar1 != "none" & input$stratdes == "none" & input$nbins > 0){
            s <- as.list(svyhist(as.formula(paste0("~", input$desvar1)), des , breaks=input$nbins))
            plotData <- data.frame(
              heights = c(s$counts, NA), 
              breaks = s$breaks, 
              mids = c(s$mids, NA))
            plotData$heights <- plotData$heights/sum(plotData$heights, na.rm=TRUE)
            w <- with(plotData, mids[2]-mids[1])
            g <- ggplot(plotData, aes(x=mids, y=heights)) + geom_bar(stat="identity", width=w) + 
              labs(y="Proportion", x = n[which(b == input$desvar1)]) + theme_bw()
            o <- ggplotly(g) 
          }
        }      
        if(exists("o")){
          o %>% layout(xaxis=list(automargin=TRUE), yaxis=list(automargin=TRUE))
        }
      }
    })
    
    output$facplot <- renderPlot({
      req(calcDescriptives())
      if(nrow(calcDescriptives()) > 2){
        tmp <- calcDescriptives()
        v <- diag((tmp$SD^2)/tmp$n)
        est <- tmp$Mean
        names(est) <- tmp$group
        f <- factorplot(est, var=v, resdf=Inf)
        plot(f, print.sig.leg=TRUE, print.square.leg=FALSE)
      }
    })
    
    
    output$Xtab <- renderDataTable({
      if(input$rowvar != "none"){
        if(input$colvar == "none"){
          out <- sumFacs(des, input$rowvar, byvar=NULL)
        } else{
          out <- sumFacs(des, input$rowvar, input$colvar)
        }    
        
        out <- datatable(out, rownames=FALSE, filter="none", 
                         options=list("paging" = FALSE, "searching"=FALSE, 
                                      "info" = FALSE)) 
        out
      }
    })    

    output$chisq <- renderUI({
      if(input$rowvar != "none"){
        if(input$colvar != "none"){
          chi2 <- X2fun(des, input$rowvar, input$colvar)
          htmltools::tags$p("The Pearson Chi-squared Statistic with the Rao & Scott Adjustment is:", 
            htmltools::br(), 
            paste0("F=", round(chi2$statistic, 2), 
            " with ", chi2$parameter[1], " numerator DF and ", chi2$parameter[2], 
            " denominator DF."), 
            htmltools::br(), 
            paste0("The p-value is approximately ", sprintf("%.3f", chi2$p.value))
          )          
          }
        }
    })    
    
    output$mosPlot <- renderPlotly({
      if(input$rowvar != "none" & input$colvar != "none"){
      mosData <- des$variables[complete.cases(des$variables[,c(input$rowvar, input$colvar)]), ]
      
      a <- paste0("aes(x=product(", input$rowvar, ", ", input$colvar, "), weight=weight,
                                      fill=", input$rowvar, ")")
      l <- length(unique(mosData[input$rowvar]))
      a <- eval(parse(text=a))
      g <- ggplot(mosData) + 
        geom_mosaic(a) +
        theme_bw() + 
        theme(legend.position="bottom") + 
        labs(x="", y="") 
      ggplotly(g)  %>% layout(legend=list(orientation="h", 
            x=ifelse(l == 2, .6, .4), y=-.05))
      }
    })
    
    output$modelInst <- renderUI({
      if(length(input$mod1) == 0){
        tags$div(tags$br(),
                 p("Model coefficients and fit statistics will appear here when you select at least one variable in the 'Model 1' 
                   selector.  Note that the default outcome you are predicting is the Liberal vote.  This can be changed with the 
                   Dependent Variable selector"),
                 tags$br())
      } else{
        tags$div(tags$br(),
                 p("The coefficient tables below provide logistic regression coefficients as the main cell entries with standard
                   errors in parentheses below.  The coefficients and standard errors are calculated using Rubin's rules with 
                   five imputed datasets."),
                 tags$br())
        
      }     
    })

    output$predprobInst <- renderUI({
      if(length(input$mod1) == 0){
        tags$div(tags$br(),
                 p("Predicted probability tables will appear here when you select at least one variable in the 'Model 1' 
                   selector.  Note that the default outcome you are predicting is the Liberal vote.  This can be changed with the 
                   Dependent Variable selector"),
                 tags$br())
      } else{
        tags$div(tags$br(),
                 p("The tables below give the predicted probability of the outcome variable for the values of the variable of interest.
                   It also gives the change in predicted probability as the variable of interest changes. "),
                 tags$br())
        
      }     
    })
    
        
    output$plotInst <- renderUI({
      if(length(input$mod1) == 0){
        tags$div(tags$br(),
                 p("A plot will appear when you select two model specifications and choose a variable of interest.  
                                  See the 'Plot' tab entry in the documentation panel for details on how this plot is constructed"),
        tags$br())
      } else{
        tags$div(tags$br(),
                 p("The plot below provides predicted probabilities of being 1 on the dependent variable for different 
                   values of the variable of interest in the specified model(s)."),
        tags$br())
      }
      
    })
    
    output$ppTab1 <- renderUI({
      req(m1())
      div(align="center", 
          br(),
          h3("Predicted Probabilities for Model 1", align="center"))
    })
    output$ppTab2 <- renderUI({
      req(m2())
      div(align="center", 
          br(), h3("Predicted Probabilities for Model 2", align="center"))
    })

    output$ppDiff1 <- renderUI({
      req(m1())
      div(align="center", 
          br(), 
          h3("Differences in Predicted Probabilities for Model 1", align="center"))
    })
    output$ppDiff2 <- renderUI({
      req(m2())
      div(align="center", 
          br(), h3("Differences in Predicted Probabilities for Model 2", align="center"))
    })
    
    
  output$desInst <- renderUI({
    if(input$desvar1 != "none"){
      
    }
    else{
      div(p("This panel provides summarise of numeric variables.  When you select a variable from the 'Summarise Variable' dropdowm, 
            a summary will appear with mean, standard deviation, inter-quartile range, quartile values, the number of valid observations
            and the number of missing observations.  If you choose a variable from the 'Group By' drop-down, the summaries will be done
            for each of the groups in the chosen variable.  In this case, a plot with means and 95% confidence intervals will also be produced"))
    }
  })

  output$xtabInst <- renderUI({
    if(input$rowvar == "none" & input$colvar == "none"){
      div(p("This panel provides frequencies and cross-tabulations of categoricalvariables.  When you select a variable from the 'Row Variable' dropdowm, 
            a frequency distribution with percentages will be produced.  If you then also choose a variable from the 'Column Variable' drop-down, a 
            contingency table will appear with cell frequencies and column percentages.   In this case, a mosaic plot will also be produced"))
      
    }
    else{
    }
  })
  
  output$varinst <- renderUI({
    if(length(input$mod1) > 0){
      if(length(input$mod2) == 0){
        ## mod1 > 0 & mod2 == 0
        inst_chc <- c(" " = "", chc[which(chc %in% input$mod1)])
      } else{
        ## mod1 > 0 & mod2 > 0
        inst_chc <- c(" " = "", chc[which(chc %in% intersect(input$mod1, input$mod2))])
        if(length(inst_chc) == 1){
          ## mod1 > 0 & mod2 > 0, no intersection
          inst_chc <- c("No Variables Common to Both Models" = "")
        }
      }
    } else{
      ## mod1 == 0
      inst_chc <- c("No Variables in Model 1" = "")
    }
    selectInput('focus_var', "Independent Variable of Interest", 
                inst_chc, selected="", selectize=TRUE, multiple=FALSE)    
    
  })
  
 output$cp1 <- renderUI({
   if(isTruthy(input$focus_var)){
   colourInput(
     "m1col", "Model 1 Colour", "#4B51B680",
     allowTransparent = TRUE)                                   
   }
})   
 output$cp2 <- renderUI({
   if(isTruthy(input$focus_var) & length(input$mod2) > 0){
     colourInput(
     "m2col", "Model 2 Colour", "#F8BA0080",
     allowTransparent = TRUE)                                   
   } 
})  
 output$lhp <- renderUI({
   #if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
      sliderInput("xpos", "Legend Horizontal Position", 
               min = 0, max=1, step=.025, value=.4)
   }
 })
 output$lvp <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     sliderInput("ypos", "Legend Vertical Position", 
               min = -.25, max=1, step=.025, value=-.2)
   }
 })
 output$sumhr <- renderUI({
   if(isTruthy(calcDescriptives())){
     br()
     br()
     br()
     hr()
   }
 })
 
 output$sumexp <- renderUI({
   if(isTruthy(calcDescriptives())){
     fname <- ifelse(input$stratdes == "none", "sum_nog.md", "sum_g.md")
     includeMarkdown(fname)
    }
 })

 output$xtexp <- renderUI({
   if(input$rowvar != "none"){
     fname2 <- ifelse(input$colvar == "none", "xtab_nog.md", "xtab_g.md")
     includeMarkdown(fname2)
   }
 })
 
 output$modexp <- renderUI({
   if(isTruthy(m1())){
     withMathJax(includeMarkdown("model_output.Rmd"))
   }
 })
 
 
 
 
 
}