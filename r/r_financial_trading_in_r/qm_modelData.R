# Create a quantmod object for use in 
# in later model fitting. Note there is 
# no need to load the data before hand. 
 
setSymbolLookup(SPY='yahoo', VXN=list(name='^VIX',src='yahoo')) 

mm <- specifyModel(Next(OpCl(SPY)) ~ OpCl(SPY) + Cl(VIX)) 
 
head(modelData(mm))


# mm is a quantmod object holding the formula nd the data structure
# implies next period open-close is modelled as function of current period open to close & current close of VIX