# compute efficient portfolio subject to target return with no short sales
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)

library(MVFPortfolio)
library(readr)
sp486 <- read_csv("~/Documents/11 Advanced Portfolio Management/Project/sp486withBB.csv")

SP486Symbols = as.list(sp486$Ticker)

begindate = "2015-07-12"
sourc ="yahoo"
assetClassSymbols =list('VTI','VO','VB','SHY','BND', 'TLT', 'TIP','MUB','VEU','VSS','VWO','VNQ','DBC','GLD')
namesClass = c(assetClassSymbols)
sp486data = AdjustedPrice(namesClass, begindate, sourc)

#bbsymbols=c("BRK-B", "BF-B")
#class(bbsymbols)
#BBdata =AdjustedPrice(bbsymbols, begindate, sourc)
#nasp500data = na.omit(sp491data)
#apply(sp487data, 2, function(x) any(is.na(x)))
#write.table(sp486data, file="sp486data.csv",sep=",",row.names=F)

#Data analysis
#library(readr)
#sp486data <- read_csv("~/Documents/11 Advanced Portfolio Management/Project/sp486data.csv")
#MVF.CornerPortfolios
sp486RR = AnnualReturnRisk(sp486data)
sp486RRCor = RSTDCorr(sp486data)
sp486CovM =CovMatrix(sp486data)
#write.table(sp486RRCor, file="sp486RRCor.csv",sep=",",row.names=F)


sp486test = sp486data
sp486testM = RRCorCov(sp486test)


#target.returnlist = c(.04, .05,.06, 0.064, .07,.08,.09,.1, .11, .12)
spTargetReturn = c(.1, .11, .12, .120727557, .13, .14, .16, .2)
#.120727557, .13
riskfree= .0293
sp486RRW = RSTDWtable(sp486data, spTargetReturn, riskfree)
spRRWplot = efplot(sp486RRW)
#if plot has margin errors, try > par("mar), and > part(mar=C(3,3,3,3))
#write.table(sp486RRW, file="sp486CornerPorts.csv",sep=",",row.names=F)
#write.table(sp486RR, file="sp486RR.csv",sep=",",row.names=T)

#pick non-zero ones
sp486nonzero = sp486RRW[, colSums(sp486RRW != 0) > 0]
write.table(sp486nonzero, file="sp486nonzero.csv",sep=",",row.names=F)
#pick the .1207 target row
targetEqPort = sp486nonzero[4,][,colSums(sp486nonzero[4,] !=0)>0]
write.table(targetEqPort, file="targetEqPort.csv",sep=",",row.names=F)
PickedOutPlot = function (fulldataset, pickdataset){
  plot(AnnualStd, AnnualMeans, main = "Securities Performances")
  text(AnnualStd, AnnualMeans, labels=rownames(AnnualStd), cex= 0.7, pos=3)
  plot(rstdwtable$Std, rstdwtable$Target_Return)

  pints(series2, col=2)
}
#sharePrice
executionDay = "2018-06-14"
targetEqSymbols = colnames(targetEqPort)[-c(1:3)]
Jun14ClosePrices = Price(targetEqSymbols, executionDay, sourc, "Close")
ncol(Jun14ClosePrices)
#RRCorr Among the selected stocks
selectedStocksData = AdjustedPrice(targetEqSymbols, begindate, sourc)
selectedStocksRRCor = RSTDCorr(selectedStocksData)
write.table(selectedStocksRRCor, file="selectedStocksRRCor.csv",sep=",",row.names=T)

##portfolio.optimalization. The global minimum, maybe?
library(PerformanceAnalytics)
library(tseries)
sp486DailyR = data.matrix(Return.calculate(sp486data)[-1,])
class(sp486DailyR)
minweights = rep(0, ncol(sp486DailyR))
optimalPort = portfolio.optim(sp486DailyR, pm = mean(sp486DailyR), reslow = minweights, rf=riskfree)
optiWeights = data.frame(t(optimalPort$pw))
optiRR = c(optimalPort$pm, optimalPort$ps)
names(optiRR)=c("Return", "Std")
print(optiRR)
names(optiWeights) = t(SP486Symbols[,1])
optiWeights[(optiWeights[1,]>0.00000001),]

#library(xlsx)
#write.xlsx(rsw2015, "/Users/Glen/Documents/11 Advanced Portfolio Management/Project/Allocation/weights.xlsx")

#write.table(arr, file="arr.csv",sep=",",row.names=F)
