## TODO
## 1. look for whether we need both c1 and c1a as well as c2 and c2a, if yes, add back in m1 and m2 from m1a and m2a

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
library(shinyjqui)
load("ces0419imp.rda")



dv.names <- c("Vote Liberal"  = "vote_lib", "Vote Conservative" = "vote_con", 
              "Vote NDP" = "vote_ndp", "Vote BQ" = "vote_bloc", 
              "Vote Green" = "vote_green", "Vote for Incumbent" = "vote_incumbent", 
              "Vote Turnout" = "turnout")
blocks <- list()
blocks[[1]] <- c("gender", "agegrp", "cathol", "relig", "educ", "union", "year_fac", "province")
blocks[[2]] <- c("continent", "market", "moral", "cynicism")
blocks[[3]] <- c("pid", "pidng", "pidnb", "pidnbg")
blocks[[4]] <- c("retroper", "retrocan")
blocks[[5]] <- c("sp_defence", "sp_envir", "immig", "usties", "jobspriv", 
                 "blame", "poorgap", "stayhome", "dowomen")
blocks[[6]] <- c("leader_con", "leader_lib", "leader_ndp", "leader_bloc", "leader_incumbent", "incumb_copart")

nblocks <- list()
nblocks[[1]] <- c("Gender", "Age Group", "Catholic", "Religion", "Schooling", "Union Household", "Year", "Region")
nblocks[[2]] <- c("Continentalism", "Market Liberalism", "Moral Tradition", "Political Cynicism")
nblocks[[3]] <- c("Party ID", "Party ID (No Green)", "Party ID (No BQ)", "Party ID (No Green or BQ)")
nblocks[[4]] <- c("Personal Retrospective", "National Retrospective")
nblocks[[5]] <- c("Defense Spend", "Environment Spend", "More Immigration",
                  "Ties to USA", "Private Sector Create Jobs", "Blame Self for Failure", 
                  "Reduce Income Inequality", "Women Stay Home", "Done for Women")
nblocks[[6]] <- c("Feelings: Conservative", "Feelings: Liberal", "Feelings: NDP", "Feelings: BQ", "Feelings: Incumbent Leader", "Incumbent Co-partisan")
b <- do.call("c", blocks)
n <- do.call("c", nblocks)

chc <- do.call("c", blocks)
names(chc) <- do.call("c", nblocks)
chc <- chc[order(names(chc))]



val_num_vars <- c("none", "market", "moral", "cynicism", "continent", 
                  "leader_bloc", "leader_lib", "leader_ndp", "leader_con")
val_num_names <- c(" ", "Market Liberalism", "Moral Traditionalism", "Political Cynicism", "Continentalism", 
                   "Feeling Thermometer: BQ Leader", "Feeling Thermometer: Liberal Leader", 
                   "Feeling Thermometer: NDP Leader", "Feeling Thermometer: Conservative")

val_strat_vars <- c("none", "gender", "agegrp", "cathol", "relig", "educ", 
                    "union", "year", "province", "pid", "retroper", 
                    "retrocan", "sp_defence", "sp_envir", "immig", "vote", 
                    "usties", "jobspriv", "blame", "poorgap", "stayhome", "dowomen", "incumb_copart")
val_strat_names <- c("None", "Gender", "Age Group", "Catholic", "Religion", "Schooling", 
                     "Union Household", "Year", "Region", "Party ID", "Personal Retrospective Economy", 
                     "National Retrospective Economy", "Defense Spending", "Environment Spending", "More Immigration?", "Vote", 
                     "Ties to USA", "Private Sector Create Jobs", "Blame Self for Failure", 
                     "Reduce Income Inequality", "Women Stay Home", "Done for Women", "Incumbent Co-partisan")
num_chc <- val_num_vars
names(num_chc) <- val_num_names
strat_chc <- val_strat_vars
names(strat_chc) <- val_strat_names
num_chc <- num_chc[order(names(num_chc))]
strat_chc <- strat_chc[order(names(strat_chc))]
wsc.none <- which(strat_chc == "none")
strat_chc <- strat_chc[c(wsc.none, (1:length(strat_chc))[-wsc.none])]


des <- svydesign(ids=~1, strata=NULL, weights=~weight, data=newdat, digits=3)
DES <- lapply(1:length(ndlist), function(i){svydesign(ids=~1, strata=NULL, weights=~weight, data=ndlist[[i]], digits=3)})

rotatedAxisElementText = function(angle,position='x'){
  angle     = as.numeric(angle[1]); 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (-angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}

pre <- function (mod1, mod2 = NULL, design, sim = FALSE, R = 2500){
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
    tab <- floor(svytable(as.formula(paste0("~", var)), d))
    tab <- tab %>% as.data.frame() %>% 
      adorn_totals("row") %>%
      adorn_percentages("col") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 0) %>%
      adorn_ns() 
      } else{
    tab <- floor(svytable(as.formula(paste0("~", var, "+", byvar)), d))
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
  #tab <- floor(svytable(as.formula(paste0("~", var, "+", byvar)), d))
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

rename.coefs <- function(obj){
  nb <- names(coef(obj))
  ints <- grep(":", nb)
  if(length(ints) > 0){
    nb <- strsplit(nb, ":", fixed=T)
  }
  for(i in 1:length(b)){
    g <- do.call('c', sapply(nb, function(nb)grep(paste0("^", b[i]), nb)))
    if(length(g) > 0){
      pat1 <- paste0(b[i], "(.*)")
      pat2 <- paste0(n[i], " (\\1)")
      nb <- lapply(nb, function(nb)gsub(pat1, pat2, nb))
    }
    nb <- lapply(nb, function(nb)gsub(" \\(\\)", "", nb))
  }
  nb <- sapply(nb, paste, collapse="x")
  names(obj$coefficients) <- nb
  return(obj)
}

mysvyHist <- function (formula, design, breaks = "Sturges", include.lowest = TRUE, 
          right = TRUE, xlab = NULL, main = NULL, probability = TRUE, 
          freq = !probability, plot=FALSE, ...) 
{
  if (inherits(design, "DBIsvydesign") || inherits(design, 
                                                   "ODBCsvydesign")) {
    design$variables <- getvars(formula, design$db$connection, 
                                design$db$tablename, updates = design$updates)
    class(design) <- "survey.design2"
  }
  mf <- model.frame(formula, model.frame(design), na.action = na.pass)
  if (ncol(mf) > 1) 
    stop("Only one variable allowed.")
  variable <- mf[, 1]
  varname <- names(mf)
  h <- hist(variable, plot = FALSE, breaks = breaks, right = right)
  props <- coef(svymean(~cut(variable, h$breaks, right = right, 
                             include.lowest = include.lowest), design, na.rm = TRUE))
  h$density <- props/diff(h$breaks)
  h$counts <- props * sum(weights(design, "sampling"))
  if (is.null(xlab)) 
    xlab <- varname
  if (is.null(main)) 
    main <- paste("Histogram of", varname)
  if(plot){plot(h, ..., freq = freq, xlab = xlab, main = main)}
  if (freq) {
    h$count_scale <- mean(diff(h$breaks)) * sum(weights(design, 
                                                        "sampling"))
  }
  invisible(h)
}


shinyServer <- function(input, output, session) {
#     Add to your server
# observeEvent(input$browser,{
#     browser()
# })

    tmpdat <- reactive({
    x <- list()
    for(i in 1:5){
      x[[i]] <- subset(DES[[i]], province %in% input$provs & year %in% input$years & !is.na(input$dv))
      for(j in 1:ncol(x[[i]]$variables)){
        if(is.factor(x[[i]]$variables[[j]])){
          x[[i]]$variables[[j]] <- droplevels(x[[i]]$variables[[j]])
        }
        if(is.labelled(x[[i]]$variables[[j]])){ x[[i]]$variables[[j]] <- as.numeric(x[[i]]$variables[[j]])}
      }
      }
    x
  })
    m1a <- reactive({
      req(input$mod1)
      iv1 <- input$mod1
      if(input$varby == "year" & isTruthy(input$focus_var)){
        iv1 <- c(iv1, paste0("year_fac*", input$focus_var))
      }
      if(input$varby == "region" & isTruthy(input$focus_var)){
        iv1 <- c(iv1, paste0("province*", input$focus_var))
      }
      if(input$varby == "incumbent" & isTruthy(input$focus_var)){
        iv1 <- c(iv1, paste0("incumb_copart*", input$focus_var))
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
            xl[[i]] <- rename.coefs(xl[[i]])
            f1[[i]] <- pre(xl[[i]], design=tmpdat()[[i]])
          }
          x <- MIcombine(xl)
          list(res=x, pre=rowMeans(sapply(f1, function(x)c(unlist(x[c("pmc", "pcp", "pre", "epmc", "epcp", "epre")])))), mod1=xl[[1]])      }
    })
    
    m2a <- reactive({
      req(input$mod2)
      iv2 <- input$mod2
      if(input$varby == "year" & isTruthy(input$focus_var)){
        iv2 <- c(iv2, paste0("year_fac*", input$focus_var))
      }
      if(input$varby == "region" & isTruthy(input$focus_var)){
        iv2 <- c(iv2, paste0("province*", input$focus_var))
      }
      if(input$varby == "incumbent" & isTruthy(input$focus_var)){
        iv2 <- c(iv2, paste0("incumb_copart*", input$focus_var))
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
          xl[[i]] <- rename.coefs(xl[[i]])
          f2[[i]] <- pre(xl[[i]], design=tmpdat()[[i]])
        }
        x <- MIcombine(xl)
        list(res=x, pre=rowMeans(sapply(f2, function(x)c(unlist(x[c("pmc", "pcp", "pre", "epmc", "epcp", "epre")])))), mod2=xl[[1]])
      }
    })


    
  c1 <- reactive({
    req(m1a())
    if(isTruthy(input$focus_var)){
    cx <- list("none" = input$focus_var, "year" = c("year_fac", input$focus_var), 
               "region" = c("province", input$focus_var), "incumbent" = c("incumb_copart", input$focus_var))
    out <- probci(m1a()$mod1, tmpdat()[[1]]$variables, changeX=cx[[input$varby]], .b=m1a()$res$coefficients, .vcov=m1a()$res$variance, numQuantVals=2, type="aveCase") 
    out     
    }
  })
  c2 <- reactive({
    req(m2a())
    if(isTruthy(input$focus_var)){
      cx <- list("none" = input$focus_var, "year" = c("year_fac", input$focus_var), 
                 "region" = c("province", input$focus_var), "incumbent" = c("incumb_copart", input$focus_var))
      out <- probci(m2a()$mod2, tmpdat()[[1]]$variables, changeX=cx[[input$varby]], .b=m2a()$res$coefficients, .vcov=m2a()$res$variance, numQuantVals=2, type="aveCase") 
    out
    }
  })

  d1 <- reactive({
    req(c1())
    x <- c1()$plot.data
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
    if(input$varby == "incumbent"){
      names(x)[1:5] <- c("incumb_copart", "x", "fit", "lower", "upper")
    }
    x    
    
  })
  
  
    d2 <- reactive({
    req(c2())
    x <- c2()$plot.data
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
    if(input$varby == "incumbent"){
      names(x)[1:5] <- c("incumb_copart", "x", "fit", "lower", "upper")
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
                              "vote_bloc" = "Bloc Quebecois", 
                              "vote_green" = "Green", 
                              "turnout" = "Turnout")
    
    if(is.factor(pd()$z)){
      if(input$varby == "none"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model)) + 
        geom_point(position=position_dodge(width=.5), size=3) + 
        geom_errorbar(width=0, position=position_dodge(width=.5), size=1.5) + 
        scale_colour_manual(values=colvec()) + 
        #xlab(n[which(b == input$focus_var)]) +
        xlab("") + 
        #ylab(paste0("Pr(Vote ", party, ")")) +
        ylab("") + 
        theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw()     
      }
      if(input$varby == "year"){
          g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model)) + 
          geom_point(position=position_dodge(width=.5), size=3) + 
          geom_errorbar(width=0, position=position_dodge(width=.5), size=1.5) + 
          scale_colour_manual(values=colvec()) + 
          #xlab(n[which(b == input$focus_var)]) +
          xlab("") + 
          #ylab(paste0("Pr(Vote ", party, ")")) +
          ylab("") + 
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~year)
       }
      if(input$varby == "region"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model)) + 
          geom_point(position=position_dodge(width=.5), size=3) + 
          geom_errorbar(width=0, position=position_dodge(width=.5), size=1.5) + 
          scale_colour_manual(values=colvec()) + 
          #xlab(n[which(b == input$focus_var)]) +
          xlab("") + 
          #ylab(paste0("Pr(Vote ", party, ")")) +
          ylab("") + 
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~region)
        }
      if(input$varby == "incumbent"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model)) + 
          geom_point(position=position_dodge(width=.5), size=3) + 
          geom_errorbar(width=0, position=position_dodge(width=.5), size=1.5) + 
          scale_colour_manual(values=colvec()) + 
          #xlab(n[which(b == input$focus_var)]) +
          xlab("") + 
          #ylab(paste0("Pr(Vote ", party, ")")) +
          ylab("") + 
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~incumb_copart)
      }
    }
    else{
      if(input$varby == "none"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model, fill=model)) + 
          geom_ribbon(alpha=.25, linetype="blank") + 
          geom_line(size=1) + 
          scale_colour_manual(values=colvec()) + 
          scale_fill_manual(values=colvec()) + 
          #xlab(n[which(b == input$focus_var)]) +
          xlab("") + 
          #ylab(paste0("Pr(Vote ", party, ")")) +
          ylab("") + 
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw()
      }
      if(input$varby == "year"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model, fill=model)) + 
          geom_ribbon(alpha=.25, linetype="blank") + 
          geom_line(size=1) + 
          scale_colour_manual(values=colvec()) + 
          scale_fill_manual(values=colvec()) + 
          #xlab(n[which(b == input$focus_var)]) +
          xlab("") + 
          #ylab(paste0("Pr(Vote ", party, ")")) +
          ylab("") + 
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~year)
      }
      if(input$varby == "region"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model, fill=model)) + 
          geom_ribbon(alpha=.25, linetype="blank") + 
          geom_line(size=1) + 
          scale_colour_manual(values=colvec()) + 
          scale_fill_manual(values=colvec()) + 
          #xlab(n[which(b == input$focus_var)]) +
          xlab("") + 
          #ylab(paste0("Pr(Vote ", party, ")")) +
          ylab("") + 
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~region)
      }
      if(input$varby == "incumbent"){
        g <- ggplot(pd(), aes(x=z, y=fit, ymin=lower, ymax=upper, colour=model, fill=model)) + 
          geom_ribbon(alpha=.25, linetype="blank") + 
          geom_line(size=1) + 
          scale_colour_manual(values=colvec()) + 
          scale_fill_manual(values=colvec()) + 
          #xlab(n[which(b == input$focus_var)]) +
          xlab("") + 
          #ylab(paste0("Pr(Vote ", party, ")")) +
          ylab("") + 
          theme(aspect.ratio=1, axis.text = element_text(size=14)) + theme_bw() + facet_wrap(.~incumb_copart)
      }
    }
    g <- g + theme(axis.text.x = rotatedAxisElementText(input$rotlabs, "x"))
    ggplotly(g, height=input$ploth, width=input$plotw) %>% 
      layout(autosize=TRUE, 
        xaxis=list( title= ""), 
        yaxis=list(title=""), 
        annotations = list(list(text = input$ytitle,
                             x = input$ytoffw,
                             y = input$ytoffh,
                             xref = "paper",
                             yref = "paper",
                             showarrow = F,
                             textangle = -90), 
                         list(text = input$xtitle,
                             x = input$xtoffw,
                             y = input$xtoffh,
                             xref = "paper", 
                             yref = "paper",
                             showarrow = F,
                             textangle = 0)),
        legend=list(orientation="h", x=input$xpos, y=input$ypos))
    }
  })
  container1 <- reactive({
    fv <- n[which(b == input$focus_var)]
    withTags(
    table(
      class = "display",
      thead(
        tr(
          th(colspan = 2, class="dt-center", "Comparisons"),
          th(colspan = 3, class="dt-center", "Statistics")
        ),
        tr(
          th(fv, class="dt-center", ),
          th(fv, class="dt-center", ),
          th("Predicted Probs"), 
          th("Lower 95% CI"), 
          th("Upper 95% CI")
        )
     )
    )
  )
})
  container2 <- reactive({
    fv <- n[which(b == input$focus_var)]
    vbv <- switch(input$varby, year = "Year", region="Region", incumbent = "Inc. Co-partisan")
    withTags(
      table(
        class = "display",
        thead(
          tr(
            th(colspan = 2, class="dt-center", "Group 1"),
            th(colspan = 2, class="dt-center", "Group 2"),
            th(colspan = 3, class="dt-center", "Statistics")
          ),
          tr(
            th(vbv),
            th(fv),
            th(vbv),
            th(fv),
            th("Predicted Probs"), 
            th("Lower 95% CI"), 
            th("Upper 95% CI")
          )
        )
      )
    )
  })
  observeEvent(d1(), {
  output$t1a <- renderDataTable({
      req(c1())
      out <- c1()$plot.data
      names(out) <- gsub(
        input$focus_var,
        n[which(b == input$focus_var)],
        names(out))
      names(out) <- gsub("year_fac", "Year", names(out))
      names(out) <- gsub("province", "Region", names(out))
      names(out) <- gsub("incumb_copart", "Incumbent Co-partisan", names(out))
      names(out) <- gsub("pred_prob", "Predicted Probability", names(out))
      names(out) <- gsub("lower", "Lower 95% CI", names(out))
      names(out) <- gsub("upper", "Upper 95% CI", names(out))
      
      DT::datatable(out, rownames=FALSE, filter="none", 
                options=list("paging" = FALSE, "searching"=FALSE, 
                             "info" = FALSE)) %>% 
        formatRound(columns=c("Predicted Probability", "Lower 95% CI", "Upper 95% CI"), digits=3)
    })
    output$t1b <- DT::renderDataTable({
      req(c1())
      out <- c1()$`Difference in Predicted Probabilities`
      names(out) <- gsub(
        paste0(input$focus_var, "(\\d)"), 
        n[which(b == input$focus_var)], 
        names(out))
      names(out) <- gsub("year_fac(\\d)", "Year", names(out))
      names(out) <- gsub("province(\\d)", "Region", names(out))
      names(out) <- gsub("incumb_copart(\\d)", "Inc. Co-partisan", names(out))
      names(out) <- gsub("pred_prob", "Predicted Probability", names(out))
      names(out) <- gsub("lower", "Lower 95% CI", names(out))
      names(out) <- gsub("upper", "Upper 95% CI", names(out))
      if(input$varby != "none"){
        x <- datatable(out, container = container2(), rownames=FALSE,
                       options=list("paging" = TRUE,
                                    "info" = FALSE, 
                                    columnDefs = list(list(targets="_all", className="dt-center"))),
                       filter="top") %>% 
          formatRound(columns=c("Predicted Probability", "Lower 95% CI", "Upper 95% CI"), digits=3)  %>% 
          formatStyle(c(1), backgroundColor="#4B51B680", `border-left` = "solid 2px") %>% 
          formatStyle(c(2), backgroundColor="#4B51B680", `border-right` = "solid 2px") %>% 
          formatStyle(c(3), backgroundColor="#F8BA0080") %>% 
          formatStyle(c(4), backgroundColor="#F8BA0080", `border-right` = "solid 2px")
      }else{
        x <- datatable(out, container = container1(), rownames=FALSE,
                       options=list("paging" = TRUE,
                                    "info" = FALSE, 
                                    columnDefs = list(list(targets="_all", className="dt-center"))),
                       filter="top") %>% 
          formatRound(columns=c("Predicted Probability", "Lower 95% CI", "Upper 95% CI"), digits=3)  %>% 
          formatStyle(c(1), backgroundColor="#4B51B680", `border-right` = "solid 2px", `border-left` = "solid 2px") %>% 
          formatStyle(c(2), backgroundColor="#F8BA0080", `border-right` = "solid 2px")
        
      }
      x
    }, 
      rownames=FALSE
      )}) 
    observeEvent(d2(), {
    output$t2a <- renderDataTable({
      req(c2())
      out <- c2()$plot.data
      names(out) <- gsub(
        input$focus_var,
        n[which(b == input$focus_var)],
        names(out))
      names(out) <- gsub("year_fac", "Year", names(out))
      names(out) <- gsub("province", "Region", names(out))
      names(out) <- gsub("incumb_copart", "Incumbent Co-partisan", names(out))
      names(out) <- gsub("pred_prob", "Predicted Probability", names(out))
      names(out) <- gsub("lower", "Lower 95% CI", names(out))
      names(out) <- gsub("upper", "Upper 95% CI", names(out))
      DT::datatable(out, rownames=FALSE, filter="none", 
                    options=list("paging" = FALSE, "searching"=FALSE, 
                                 "info" = FALSE)) %>% 
        formatRound(columns=c("Predicted Probability", "Lower 95% CI", "Upper 95% CI"), digits=3)
    })
    
    output$t2b <- DT::renderDataTable({
      req(c2())
      out <- c2()$`Difference in Predicted Probabilities`
      names(out) <- gsub(
        paste0(input$focus_var, "(\\d)"), 
        n[which(b == input$focus_var)], 
        names(out))
      names(out) <- gsub("year_fac(\\d)", "Year", names(out))
      names(out) <- gsub("province(\\d)", "Region", names(out))
      names(out) <- gsub("incumb_copart(\\d)", "Inc. Co-partisan", names(out))
      names(out) <- gsub("pred_prob", "Predicted Probability", names(out))
      names(out) <- gsub("lower", "Lower 95% CI", names(out))
      names(out) <- gsub("upper", "Upper 95% CI", names(out))
      
      if(input$varby != "none"){
        x <- datatable(out, container = container2(), rownames=FALSE,
                          options=list("paging" = TRUE,
                                       "info" = FALSE,
                                       columnDefs = list(list(targets="_all", className="dt-center"))),
                          filter="top") %>% formatRound(columns=c("Predicted Probability", "Lower 95% CI", "Upper 95% CI"), digits=3) %>% 
          formatStyle(c(1), backgroundColor="#4B51B680", `border-left` = "solid 2px") %>% 
          formatStyle(c(2), backgroundColor="#4B51B680", `border-right` = "solid 2px") %>% 
          formatStyle(c(3), backgroundColor="#F8BA0080") %>% 
          formatStyle(c(4), backgroundColor="#F8BA0080", `border-right` = "solid 2px")
      } else {
        x <- datatable(out, container=container1(), rownames=FALSE,
                                  options=list("paging" = TRUE,
                                               "info" = FALSE,
                                               columnDefs = list(list(targets="_all", className="dt-center"))),
                                  filter="top") %>% formatRound(columns=c("Predicted Probability", "Lower 95% CI", "Upper 95% CI"), digits=3)%>% 
          formatStyle(c(1), backgroundColor="#4B51B680", `border-right` = "solid 2px", `border-left` = "solid 2px") %>% 
          formatStyle(c(2), backgroundColor="#F8BA0080", `border-right` = "solid 2px")
      }
      
      x
      }, 
      rownames=FALSE
    ) })
    
    output$mo1 <- renderUI({
      s <- NULL
      if(length(input$mod1) > 0 & length(input$mod2) == 0){
        s <- stargazer(m1a()$mod1, notes.append=FALSE,
                      star.cutoffs = .05,
                      notes="*p < 0.05, two-sided",
                      dep.var.labels.include = FALSE, 
                      dep.var.caption = names(dv.names)[which(dv.names == input$dv)], 
                      coef=list(coef(m1a()$res)),  
                      se = list(sqrt(diag(vcov(m1a()$res)))),
                      type="html", keep.stat="n")
        }
      if(length(input$mod1) == 0 & length(input$mod2) > 0){
        s <- stargazer(m2a()$mod2, notes.append=FALSE,
                       star.cutoffs = .05,
                       notes="*p < 0.05, two-sided", 
                       dep.var.labels.include = FALSE, 
                       dep.var.caption = names(dv.names)[which(dv.names == input$dv)], 
                       coef=list(coef(m2a()$res)),
                       se = list(sqrt(diag(vcov(m2a()$res)))),
                       type="html", keep.stat="n")
      }
      if(length(input$mod1) > 0 & length(input$mod2) > 0){
        s <- stargazer(m1a()$mod1, m2a()$mod2, notes.append=FALSE,
                       star.cutoffs = .05,
                       notes="*p < 0.05, two-sided",
                       dep.var.labels.include = FALSE, 
                       dep.var.caption = names(dv.names)[which(dv.names == input$dv)], 
                       coef=list(coef(m1a()$res), coef(m2a()$res)),  
                       se = list(sqrt(diag(vcov(m1a()$res))), sqrt(diag(vcov(m2a()$res)))),
                       type="html", keep.stat="n")
      }
      if(!is.null(s)){
        HTML(s)
      }
      })
    
    output$mfs <- renderUI({
      req(m1a())
      div(br(),
      h3("Model Fit Statistics"))

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
      if(input$desvar != "none" & input$stratdes == "none"){
      out <- mysvyHist(as.formula(paste0("~", input$desvar)), des)
      out <- as.list(out)
      out
      }
    })
    
    calcDescriptives <- reactive({
      if(input$desvar != "none"){
        if(input$stratdes != "none"){
          numOut <- sumStats(des, input$desvar, byvar=input$stratdes)
        } 
        else{
          numOut <- sumStats(des, input$desvar)    
        }
        rownames(numOut) <- NULL
        numOut      
      }
    })
    
    
    output$Descriptive <- renderDataTable({
      req(calcDescriptives())  
      out <- datatable(calcDescriptives(), rownames=FALSE, 
                         options=list("paging" = FALSE, "searching"=FALSE, "info"=FALSE)) %>% 
                         formatRound(columns=c("Mean", "SD", "IQR", "0%", "25%", "50%", "75%", "100%"), digits=3) %>%
                         formatRound(columns=c("n", "NA"), digits=0)
      out
      })    
    
    output$histPlot <- renderPlotly({
      req(sHist())
    })
    
    output$binInput <- renderUI({
      if(input$desvar != "none" & input$stratdes == "none"){
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
          if(input$desvar != "none" & input$stratdes == "none" & input$nbins > 0){
            s <- as.list(mysvyHist(as.formula(paste0("~", input$desvar)), des , breaks=input$nbins))
            plotData <- data.frame(
              heights = c(s$counts, NA), 
              breaks = s$breaks, 
              mids = c(s$mids, NA))
            plotData$heights <- plotData$heights/sum(plotData$heights, na.rm=TRUE)
            w <- with(plotData, mids[2]-mids[1])
            g <- ggplot(plotData, aes(x=mids, y=heights)) + geom_bar(stat="identity", width=w) + 
              labs(y="Proportion", x = n[which(b == input$desvar)]) + theme_bw()
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
            " with ", round(chi2$parameter[1],2), " numerator DF and ", round(chi2$parameter[2], 2),
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
      req(m1a())
      if(isTruthy(input$focus_var)){
      div(align="center", 
          br(),
          h3("Predicted Probabilities for Model 1", align="center"))
      }
    })
    output$ppTab2 <- renderUI({
      req(m2a())
      if(isTruthy(input$focus_var)){
        div(align="center", 
          br(), h3("Predicted Probabilities for Model 2", align="center"))
      }
    })

    output$ppDiff1 <- renderUI({
      req(m1a())
      if(isTruthy(input$focus_var)){
        div(align="center", 
          br(), 
          h3("Differences in Predicted Probabilities for Model 1", align="center"))
      }
    })
    output$ppDiff2 <- renderUI({
      req(m2a())
      if(isTruthy(input$focus_var)){
        div(align="center", 
          br(), h3("Differences in Predicted Probabilities for Model 2", align="center"))
      }
    })
    
    
  output$desInst <- renderUI({
    if(input$desvar != "none"){
      
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
 
 output$ploth <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", numericInput("ploth", "height", 
                                                    min = 400, max=1000, step=10, value=650))
   }
 })
 output$plotw <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", numericInput("plotw", "width", 
                                                    min = 400, max=1000, step=10, value=650))
   }
 })
 output$plotdimx <- renderUI({
   if(isTruthy(input$focus_var)){
     div(class="row", HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),div(style="font-weight:bold", 
                                                     "Plot Dimensions (px)")) 
   }
 })

 output$xtoffh <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", numericInput("xtoffh", "y", 
                                                    min = -.25, max=1, step=.01, value=-.1))
   }
 })
 output$xtoffw <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", numericInput("xtoffw", "x", 
                                                    min = -.25, max=1, step=.01, value=.5))
   }
 })
 output$xtoffdimx <- renderUI({
   if(isTruthy(input$focus_var)){
     div(class="row", HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),div(style="font-weight:bold", 
                                                           "X title offset")) 
   }
 })

 output$ytoffh <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", numericInput("ytoffh", "y", 
                                                    min = -.25, max=1, step=.01, value=0.5))
   }
 })
 output$ytoffw <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", numericInput("ytoffw", "x", 
                                                    min = -.25, max=1, step=.01, value=-.075))
   }
 })
 output$ytoffdimx <- renderUI({
   if(isTruthy(input$focus_var)){
     div(class="row", HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),div(style="font-weight:bold", 
                                                           "Y title offset")) 
   }
 })
 
 output$rotlabs <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", numericInput("rotlabs", "Rotate x-axis labels", 
                                                    min = -180, max=180, step=1, value=0))
   }
 })

 output$xtitle <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     div(style="display:inline-block", textInput("xtitle", "X title", value=n[which(b == input$focus_var)]))
   }
 })

 output$ytitle <- renderUI({
   # if(length(input$mod1) > 0 | length(input$mod2) > 0){
   if(isTruthy(input$focus_var)){
     party <- switch(input$dv, 
                     "vote_lib" = "Liberal", 
                     "vote_con" = "Conservative", 
                     "vote_ndp" = "NDP", 
                     "vote_bloc" = "Bloc Quebecois", 
                     "vote_green" = "Green", 
                     "turnout" = "Turnout")
     div(style="display:inline-block", textInput("ytitle", "Y title", value=paste0("Pr(Vote ", party, ")")))
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
   if(isTruthy(m1a())){
     withMathJax(includeMarkdown("model_output.Rmd"))
   }
 })
 
 desc.steps <- reactive({
   b <- do.call("c", blocks)
   nm <- do.call("c", nblocks)
   data.frame(
   element=c("#desvar + .selectize-control", "#Descriptive", "#descPlot", "#binInput", "#stratdes + .selectize-control", "#Descriptive", "#descPlot", "#facplot"),
   intro = c("Choose a variable to summarise - pick one from the list to see what happens.",
             "The output here shows sample summary statistics for the selected variable for all observations.",
             "The histogram chooses the distribution of the selected variable.",
             "Use this input to change the number of bars in the histogram to get a different resolution. Change this to 15 to see what happens.",
             "Choose a variable to stratify the summary statistics. The summaries will then be done for each unique value of the variable you pick. Pick 'Age Group' to see what happens.",
             "Notice that when you choose a grouping variable, there is now one line of summary statistics for each value of the grouping variable.",
             "When you choose a grouping variable, like Age Group, that changes the plot from a histogram to an error-bar plot. The points give the average of the selected variable for each value of Age Group. The lines give the 95% confidence interval.",
             "This plot shows whether each group is statistically different from all of the others. A white box means that the difference between the row and column groups are not significant; gray boxes mean the differences are significant."),
   position = c("top", "bottom", "right","bottom", "right", "bottom", "top", "top"), stringsAsFactors=FALSE
 )})

 xt.steps <- reactive({
   data.frame(
     element = c("#rowvar + .selectize-control", "#Xtab", "#colvar + .selectize-control", "#Xtab", "#chisq", "#mosPlot"), 
     intro = c(
       "Choose a variable for the rows of the contingency table - pick one to see what happens. This would be your dependent variable.",
       "When there is no column variable, you see frequency (counts) and relative frequency (percentages) distributions of the selected variable. The entries are counts and column proportions.",
       "Choose a variable for the columns of the table. This would be your independent variable.",
       "When you choose a column variable, you get a contingency table with cell counts and column percentages.",
       "When you choose a column variable, you also get a Chi-squared statistic and its p-value for independence tests.",
       "Finally, when you choose a column variable, you also get a mosaic plot. This is a visual representation of the contingency table. The width of each column is proportional to the number in each column-variable group. The height of each bar within the column is a representation of the column proportion."
     ), 
     position = c("right", "right", "right", "top", "top", "left")
   )
 })
 
 model.steps <- reactive({
   data.frame(
   element = c("#dv", "#mod1 + .selectize-control", "#subset", "#mo1", "#mo2", "#varinst",  "#p1", "#t1a", "#t1b", "#chkbox", "#mod2 + .selectize-control", "#modOutput", "#varinst", "#p1", "#btnOutput", "#colPickers", "#legPos", "#plotSizeCtrl", "#xto", "#yto", "#rotlabs", "#axTitles", "#varby", "#p1", "#modOutput","#pp1a", "#pp1b"),
   intro = c("First, pick a dependent variable to model. These include votes for specific parties, for the incumbent and vote turnout. Pick one now to see how it works.",
             "You need to specify at least one independent variable in model 1 to get model output. Pick one or more variables now to see what the output looks like.",
             "You can use the check boxes to subset the data. The observations from the unchecked groups will be removed from the analysis.",
             "The main entries in the table are logistic regression coefficients calculated using survey weights to produce nationally representative predictions. The standard errors of the coefficients are in parentheses. These coefficients help us understand which effects are statistically significant. However, there are better ways to understand the substantive impact of a variable. Hit 'Next' to find out how.",
             "This table gives you model fit statistics. The Proportional Reduction in Error (PRE) tells you by what proportion you reduce prediction errors by your model. The expected PRE is similar, but takes into account the size of the predicted probabilities (see descriptive note below). These values have an upper bound of 1, but are not bounded on the lower end at 0. Zero is the PRE you would get from a model with no independent variables in it.  Numbers 0 or less mean you are better of with no model rather than the one you estimated.  For more on how to interpret and write about these measures, see the writing guide.",
             "You can choose a variable from this list to learn more about its effect on the dependent variable. The results do not show up here, but in the “Plot” and “Predicted Probabilities” tabs. If you have estimated only one model, this list is automatically populated with all of the variables in the model. Choose a variable and hit the 'Next' button to see them.",
             "The plot that is generated here shows the predicted probability of engaging in the dependent variable activity (turnout, vote incumbent or vote for a particular party) for each of the values of the independent variable you just chose. The points represent the probabilities and the lines give the 95% confidence interval for the probability. See the “Documentation” tab for more details on how this was calculated. There are some controls for the plot, but we'll talk more about them in a minute. Hit the 'Next' button to see more results. The plot is made with plotly, so hovering your mouse over the top of the plot will activate lots of interactive options including exporting it as a .png file.",
             "The top table gives the predicted probabilities. These are the same data that were used to make the plot you just saw. Hit the 'Next' button to get a description of the table below.",
             "This table gives the differences in predicted probability for any two values of the variable you chose to plot. This allows you to test hypotheses about the difference in predicted probability between any two levels. Hit the 'Next' button to add another model to compare to the one you just estimated. Let's go back and add another model for comparison.",
             "You can also estimate a comparison model. Click the check box to enable the comparison model selector", 
             "When you specify the second model, it can have all, some, or none of the same variables as model 1. However, once two models are estimated, the independent variable of interest list is automatically populated with only the variables that are common to both models.  Choose some variables for the second model now and then hit the 'Next' button.",
             "The coefficients and fit statistics mean the same thing as before, there are just two sets of them now.",
             "Choose a variable of interest again from the variables that are common to both models. Pick one now and then hit the 'Next' button.",
             "Now the plot has two colors - one for each different model. The points are still predicted probabilities and the lines are 95% confidence intervals for those predicted probabilities.",
             "Clicking this button will make available a number of different controls for the plot visible. Given all of the different options, it is difficult to make it look perfect automatically, so there are lots of controls here to help you make the plot look the way you want. Click the 'Show Plot Controls' button now; you will be guided through the different controls when you hit 'Next' in the tooltip.",
             "These are colour pickers for the points and lines from the first and models. By clicking this, you can interactively define the colour hue and transparency. Give it a try!",
             "As the plots generated can end up being different sizes, the legend does not always show up in exactly the right place. These sliders control the horizontal and vertical position of the legend. Moving the sliders to the right move the legend to the left and top. You can move the legend into the plotting region if you like.",
             "Changing these values will change the height and width of the plot that you see. The units of these numbers is pixels. Note that the plot doesn't have to be square",
             "These input boxes allow you to move the title on the x-axis in vertical (y) and horizontal (x) directions.",
             "These input boxes allow you to move the title on the y-axis in vertical (y) and horizontal (x) directions.",
             "Particularly when effects are varied by region or year (more on that in a few steps), the x-axis tick mark labels sometimes over-plot each other. This can be solved by rotating the axis labels. The number you put in here rotates them that many degrees counter-clockwise.",
             "These input boxes allow you to change the title of the x- and y-axes. This includes the possibility of making them blank by deleting everything in the box.",
             "By choosing year or region, you can vary the size of the effect of the independent variable of interest by that variable.",
             "Notice now that there is (are) an interaction(s) between the 'Vary Effect by' variable and the 'Independent Variable of Interest'. These interaction coefficients and the main effects of the variables can be seen in the coefficient table. This also changes all of the other model output, too. Note, that for this to work, an independent variable of interest must be selected.",
             "The plot is similar to the one before with all of the same controls. The difference is that there are either 4 panels for region or 6 panels for year.  The region or year are identified in the light gray strip above each panel.",
             "These are again the predicted probabilities, but this time they are for every combination of the independent variable of interest and either year or region, depending on which one you chose.",
             "The differences in predicted probability table shows the difference in predicted probability for every combination of the variable of interest and region or year and every other combination of those same two variables. You'll notice that there is a 'page' selector at the bottom of the table as the table can be quite long. The boxes underneath the table headings can be used to filter the results. By clicking in the boxes, you can select the elements you want to keep (for categorical variables) or you can specify a range for continuous variables."),
   position = c("right", "right", "right", "top", "top", "right", "right", "left", "top", "right", "right", "top", "right", "right", "top", "right", "right", "right", "right", "right", "right", "right", "right", "right", "right", "bottom", "top"))
 })
 
 
 output$btnOutput <- renderUI({
   if(isTruthy(input$focus_var)){
    HTML('<br/><button id="ctrlButton" onclick="myFunction()">Show Plot Controls</button>')
   }
  })
 observeEvent(input$btn,
              introjs(session, options = list(steps=desc.steps())))
 
 observeEvent(input$btn2,
              introjs(session, options = list(steps=xt.steps())))
 observeEvent(input$btn3,
              introjs(session, options = list(steps=model.steps()), 
                      events = list(onbeforechange = readCallback("switchTabs"))))
 
 

 # outputOptions(output, "t1a", suspendWhenHidden=FALSE, priority=8)
 # outputOptions(output, "t1b", suspendWhenHidden=FALSE, priority=7)
 # outputOptions(output, "t2a", suspendWhenHidden=FALSE, priority=6)
 # outputOptions(output, "t2b", suspendWhenHidden=FALSE, priority=5)
 outputOptions(output, "p1", suspendWhenHidden=FALSE)
 outputOptions(output, "mo1", suspendWhenHidden=FALSE, priority=10)
 outputOptions(output, "mo2", suspendWhenHidden=FALSE, priority=9)

}