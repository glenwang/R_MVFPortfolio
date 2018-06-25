if(!require(IntroCompFinR)){
  install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
  library(IntroCompFinR)
}

if(!require(quantmod)){
  install.packages("quantmod")
  library(quantmod)
}

if(!require(PerformanceAnalytics)){
  install.packages("PerformanceAnalytics")
  library(PerformanceAnalytics)
}

if(!require(tibble)){
  install.packages("tibble")
  library(tibble)
}

AdjustedPrice= function(symbol_list, fromtime, sourc){
  library(quantmod)
  dataset=xts()
  for(i in symbol_list) {
    cat("Downloading time series for symbol '", i, "' ...\n",
        sep = "")
    status <- tryCatch(getSymbols(i, src = sourc,
                                  from = as.Date(fromtime)),
                       error = identity)
    if (grepl("-", i)){
      idata = eval(parse(text = paste("`", i, "`", sep = "")))
      i = gsub("-", ".", i)
      colnames(idata) = gsub("-", ".", colnames(idata))
      cat("Replacing dash in", i, sep = " ")
      assign(i, idata)
      #return(eval(parse(text = i)))
    }
    adj= paste(i,"$",i,".Adjusted", sep = "")
    print(paste("Merging", adj, sep = " "))
    dataset <- merge(dataset, eval(parse(text = adj)))
    if(inherits(status, "error"))
      cat("Symbol '", i, "' not downloadable!\n", sep = "")
  }
  names(dataset) = c(symbol_list)
  colnames(dataset) = gsub("-", ".", colnames(dataset))
  return(dataset)
}

Price = function(symbol_list, fromtime, sourc, colmn){
  library(quantmod)
  dataset=xts()
  for(i in symbol_list) {
    cat("Downloading time series for symbol '", i, "' ...\n",
        sep = "")
    status <- tryCatch(getSymbols(i, src = sourc,
                                  from = as.Date(fromtime)),
                       error = identity)
    if (grepl("-", i)){
      idata = eval(parse(text = paste("`", i, "`", sep = "")))
      i = gsub("-", ".", i)
      colnames(idata) = gsub("-", ".", colnames(idata))
      cat("Replacing dash in", i, sep = " ")
      assign(i, idata)
      #return(eval(parse(text = i)))
    }
    adj= paste(i,"$",i,".", colmn, sep = "")
    print(paste("Merging", adj, sep = " "))
    dataset <- merge(dataset, eval(parse(text = adj)))
    if(inherits(status, "error"))
      cat("Symbol '", i, "' not downloadable!\n", sep = "")
  }
  names(dataset) = c(symbol_list)
  colnames(dataset) = gsub("-", ".", colnames(dataset))
  return(dataset)
}

##TBD

AnnualExpectedReturn = function(adjustedprice_dataset){
  library(PerformanceAnalytics)
  returns=xts()
  returns = (Return.calculate(adjustedprice_dataset))
  means=Return.annualized(returns)
  means = t(means)
  AnnualMeans = data.matrix(means)
  return(AnnualMeans)
}

AnnualReturnRisk = function(adjustedprice_dataset){
  library(PerformanceAnalytics)
  returns=xts()
  ReturnRisk=xts()
  returns = (Return.calculate(adjustedprice_dataset))
  means=Return.annualized(returns)
  annaulStd= StdDev.annualized(returns)
  Means = t(means)
  AnnualMeans = data.matrix(Means)
  annStd = t(annaulStd)
  AnnualStd= data.matrix(annStd)
  plot(AnnualStd, AnnualMeans, main = "Securities Performances")
  text(AnnualStd, AnnualMeans, labels=rownames(AnnualStd), cex= 0.7, pos=3)
  ReturnRisk=data.frame(AnnualMeans,AnnualStd)
  names(ReturnRisk) = c("Return","Std")
  return(ReturnRisk)
}
#######

CorrMatrix = function (adjustedprice_dataset){
  corr_matrix=xts()
  returns = Return.calculate(adjustedprice_dataset)
  #returns = data.frame(diff(as.matrix(log(adjustedprice_dataset))))
  corr_matrix=data.frame(cor(returns,use = "complete.obs"))
  return(corr_matrix)
}

RSTDCorr = function (adjustedprice_dataset){
  inrr = AnnualReturnRisk(adjustedprice_dataset)
  incorr = CorrMatrix(adjustedprice_dataset)
  rstdcorrtable = cbind(inrr, incorr)
  return(rstdcorrtable)
}

CovMatrix = function(adjustedprice_dataset){
  cov_matrix =xts()
  returns = Return.calculate(adjustedprice_dataset)
  #returns = data.frame(diff(as.matrix(log(adjustedprice_dataset))))
  cov_matrix=data.frame(cov(returns, use = "complete.obs"))
  return(cov_matrix)
}

RRCorCov = function(adjustedprice_dataset){
  aRR <- AnnualReturnRisk(adjustedprice_dataset)
  assign(paste(deparse(substitute(adjustedprice_dataset)), "RR", sep = "."), aRR, envir = .GlobalEnv)
  RRCor <- RSTDCorr(adjustedprice_dataset)
  assign(paste(deparse(substitute(adjustedprice_dataset)), "RRCor", sep = "."), RRCor, envir = .GlobalEnv)
  Cov <- CovMatrix(adjustedprice_dataset)
  assign(paste(deparse(substitute(adjustedprice_dataset)), "Cov", sep = "."), Cov, envir = .GlobalEnv)
}

######
RSTDWtable = function(adjustedprice_dataset, target.returnlist, r.free){
  library(tibble)
  library(IntroCompFinR)
  symbols = colnames(adjustedprice_dataset)
  covmtri =CovMatrix(adjustedprice_dataset)
  annuerMatrix = AnnualExpectedReturn(adjustedprice_dataset)
  annuer = c(annuerMatrix)
  names(annuer)= c(symbols)
  outputtable = xts()
  for (i in target.returnlist){
    efpf = efficient.portfolio(annuer, covmtri, i, shorts=FALSE)
    a =data.frame(t(data.matrix(unlist(summary(efpf, risk.free=r.free)))))
    a = a[,-1]
    rstd = a[,1:2]
    names(rstd) = c("Target_Return","Std")
    rstd = data.matrix(rstd)
    weights = a[,-c(1:2)]
    names(weights) = colnames(adjustedprice_dataset)
    weights = data.matrix(weights)
    outputrow = data.frame(cbind(rstd,weights))
    #print(outputrow)
    outputtable = data.frame(rbind(outputtable,outputrow))
    plot(efpf, col="blue" )
    mtext(paste("Target Return =",i, sep = " "))
  }
  nuReturn =as.numeric(outputtable$Target_Return)
  nuStd =as.numeric(outputtable$Std)
  Sharpe = (nuReturn-r.free)/nuStd
  Sharpe = data.matrix(Sharpe)
  firsttwo = data.matrix(outputtable[,1:2])
  latters = data.matrix(outputtable[,-c(1:2)])
  newColum= data.frame(cbind(firsttwo,Sharpe))
  foutput =data.frame(cbind(newColum, latters))
  colnames(foutput)[3] <- "Sharpe"
  return(foutput)
}

efplot = function (rstdwtable){
  plot(rstdwtable$Std, rstdwtable$Target_Return, ylab=names(rstdwtable)[1], type = "o", xlab=names(rstdwtable)[2],
       main = paste("Efficient Frontier", deparse(substitute(rstdwtable)), sep = " "))
  text(rstdwtable$Std, rstdwtable$Target_Return, labels=rstdwtable$Target_Return, cex= 0.7, pos=4)
  }

