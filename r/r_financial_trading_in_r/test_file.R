# quantmod - wrapper for sourcing fin timeseries data
# TTR - implements specific techincal indicators
require(quantmod, TTR)

### getSymbols() gets data from variety of sources
#   current src methods - yahoo, google, MySQL, FRED (Federal Reserve Bank of St Louis), OANDA (FX & Metals), csv & RData
#   from = 'YYYY-MM-DD', to = 'YYY-MM-DD'
#   adjust = T/F 
#   return.class arguement is the class of vector returned ts, its, zoo, xts or timeSeries 
#   auto.assign = F makes it so that the timeseries isn't loaded into the lookup name, it must be explicitly set

### setDefaults() sets defaults for specific functions ie. setDefaults(getSymbols, verbose =T, src = 'mySQL')

### setSymbolLookup() sets specific sources (per above src methods) for getSymbols()

### can showSymbols(), removeSymbols(), saveSymbols(file.path)

getSymbols('IOZ.AX', from = "2000-01-01", to = Sys.Date(), src = 'yahoo', adjust = T)

plot(OpCl(IOZ.AX)) ## graphics package, Cl must call quantmod to set plot environment

lines(SMA(LoHi(IOZ.AX), n = 200), col = "red") ## graphics package, SMA (from TTR) must access the Cl(XXX) object and get point data


