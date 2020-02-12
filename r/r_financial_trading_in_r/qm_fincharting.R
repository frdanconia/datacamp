# the whole series 
chartSeries(GS) 

# now - a little but of subsetting 
# (December '07 to the last observation in '08) 
candleChart(GS,subset='2007-12::2008') 

# slightly different syntax - after the fact. 
# also changing the x-axis labeling 
candleChart(GS,theme='white', type='candles') 
reChart(major.ticks='months',subset='first 16 weeks') 

addEnvelope()
addSMA()
addROC()


# addTA allows you to add basic indicators 
# to your charts - even if they aren't part 
# of quantmod. 
 
chartSeries(to.quarterly(GS), TA=NULL) 

#Then add the Open to Close price change 
#using the quantmod OpCl function 
 
addTA(OpCl(GS),col='blue', type='h') 

addTA(OpCl(GS)) 

# Using newTA it is possible to create your own 
# generic TA function --- let's call it addOpCl 
# 
addOpCl <- newTA(OpCl,col='green',type='h') 

addOpCl() 

addOpCl <- newTA(OpCl) 