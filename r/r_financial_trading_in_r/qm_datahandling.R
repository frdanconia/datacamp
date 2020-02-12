## quantmod and xts - data handling examples

getSymbols("GS", src = "yahoo") #Goldman OHLC from yahoo 

# beyond ohlc obviousness
OpCl() #daily percent change from open to close
OpOp() #one period open to open percent change
HiCl() #percent change from high to close

# they use  three functions
Lag()  #previous value in series
Next() #next value in series
Delt() #compute change from two prices

# examples
Lag(Cl(GS)) #One period lag of the close 
Lag(Cl(GS),c(1,3,5)) #One, three, and five period lags 
Next(OpCl(GS)) #The next periods open to close - today! 

# Open to close one-day, two-day and three-day lags 
Delt(Op(GS),Cl(GS),k=1:3)

# subsetting using :: operator in the form CCYY-MM-DD HH:MM:SS
GS['2007']
GS['2008-01'] #now just January of 2008 
GS['2007-06::2008-01-12'] #Jun of 07 through Jan 12 of 08 
GS['::'] # everything in GS 
GS['2008::'] # everything in GS, from 2008 onward 

non.contiguous <- c('2007-01','2007-02','2007-12') 
GS[non.contiguous]

# using first() and last() operators
last(GS) #returns the last obs. 
last(GS,8) #returns the last 8 obs. 
last(GS, '3 weeks') 
last(GS, '-3 weeks') # all except the last 3 weeks 
last(GS, '3 months') 
last(first(GS, '2 weeks'), '3 days')

# aggregating to a different timescale
periodicity(GS) 
unclass(periodicity(GS)) 
to.weekly(GS) 
to.monthly(GS) 
periodicity(to.monthly(GS)) 
ndays(GS); nweeks(GS); nyears(GS)

# identifying endpoints and period.apply's
endpoints(GS,on="months") 
 
# find the maximum closing price each week 
apply.weekly(GS,FUN=function(x) { max(Cl(x)) } ) 
 
# the same thing - only more general 
period.apply(GS,endpoints(GS,on='weeks'), FUN=function(x) { max(Cl(x)) } ) 

# same thing - only 50x faster! 
as.numeric(period.max(Cl(GS),endpoints(GS,on='weeks')))


# Quick returns - quantmod style 
dailyReturn(GS['2007']) # returns by day 
weeklyReturn(GS['2007']) # returns by week 
monthlyReturn(GS['2007']) # returns by month, indexed by yearmon 
 
# daily,weekly,monthly,quarterly, and yearly 
allReturns(GS['2007']) # note the plural

periodReturn(GS,period='quarterly',subset='2003::')
periodReturn(GS,period='quarterly',subset='2003::', indexAt="yearqtr")
