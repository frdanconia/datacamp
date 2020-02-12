require(quantmod)

setSymbolLookup(YHOO='google',GOOG='yahoo')
setSymbolLookup(DEXUSJP='FRED')
setSymbolLookup(XPTUSD=list(name="XPT/USD",src="oanda"))
saveSymbolLookup(file="mysymbols.rda")

# new sessions call loadSymbolLookup(file="mysymbols.rda")
getSymbols(c("YHOO","GOOG","DEXUSJP","XPTUSD"))

# charting ability in quantmod --> lineChart(), barChart(), candleChart()
barChart(YHOO)
lineChart(YHOO)
candleChart(YHOO, multi.col = T, theme = "white") #multi coloring of chart & specific color themes

# chartSeries() to create standard financial charts given a ts object - reChart() redrafts plot
chartSeries(to.weekly(XPTUSD), name = "Platium (.oz) in $USD", up.col="green", dn.col="red")

# use par(ask=TRUE) to require user to advance plots, set back default par(ask=FALSE) on exit


# source this into R
# and simply call
# chart.ex()

`chart.ex` <-
  function() {
    par(ask=TRUE)
    require(quantmod)
    getSymbols("AAPL",src="yahoo")
    barChart(AAPL)
    addMACD()
    candleChart(AAPL,multi.col=TRUE,theme="white")
    getSymbols("XPT/USD",src="oanda") 
    chartSeries(XPTUSD,name="Platinum (.oz) in $USD")
    chartSeries(to.weekly(XPTUSD),name="Platinum (.oz) in $USD",up.col="white",dn.col="blue")
    par(ask=FALSE)
  }
