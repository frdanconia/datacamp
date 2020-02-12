## From Torgo - Data Mining with R - Learning with Case Studies (2011)
## Using quantmod package to specify and build models

library(xts)
library(quantmod)
library(TTR)

data(GSPC, package="DMwR2")

## Can also:
# library(quantmod)
# GSPC <- getSymbols("^GSPC", src = "yahoo", from = "1970-01-02", to = "2016-01-25", auto.assign = F)


## is future sentiment technical indicator for when future is known (ie find a backward looking indicator that reproduces this and you can hold your trades for 10 days)

## T.ind is target indicator function - parses looking for 2.5% returns for following 10 days
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) 
  {
  v <- apply(HLC(quotes), 1, mean) # averages HLC from quotes by row and puts into v
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes)) # creates matrix with quotes dates as rows, and no of days as cols

  # calcs delta between v today and v x periods in front, then moves the columns up by x periods so that 
  # all leading x(1:10) future variations from today are in todays row
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x) 

  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin])) # summing all percentage ups/downs that are > 2.5%
  if (is.xts(quotes))
      xts(x, time(quotes))
  else x
}

candleChart(last(GSPC, "2 months"), theme = "white", TA = NULL)

avgPrice <- function(p) apply(HLC(p), 1, mean)

addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")

addAvgPrice(on = 1)
addT.ind()

## now trying to find predictors using different technical indicators

# tehcnical indicators set up with some post processing for certain values from the indicators
myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1] 
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
#myEMV <- function(x) EMV(cbind(HiLo(x), Vo(x)))[, 2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(HLC(x), Vo(x))
mySAR <- function(x) SAR(cbind(Hi(x), Cl(x)))[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[, 1]


## now setting up the predictive model witha random forest
data(GSPC, package="DMwR2")
library(randomForest)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
                               + myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC)
                               + myBB(GSPC) + myCLV(GSPC) + myChaikinVol(GSPC)
                               + CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC)))
                               + myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC))
                               + mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)

rf <- buildModel(data.model, method='randomForest', training.per=c("1995-01-01","2005-12-30"), ntree=1000, importance=T)


## now we look at what out of the indicators used, what is important to the predictor

varImpPlot(rf@fitted.model, type = 1)

imp <- importance (rf@fitted.model, type=1)
rownames(imp)[which(imp>30)]

# specify model with final choices

data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k=9) + Delt(Cl(GSPC), k=10) + myATR(GSPC) + mySMI(GSPC)                            
                                        + myADX(GSPC) + myAroon(GSPC) + myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC)
                                        + mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
