mcmc.plot = function(mcmcList){
    m = as.matrix(mcmcList, chains=T, iters=T)
    # we don't want the chain and iter columns
    vars = colnames(m)
    vars = vars[vars != "CHAIN" & vars != "ITER"]
    par(mfrow=c(length(vars),2), mar=c(5, 5, 1, 1) + 0.1)
    text_size = 1.5
    for(i in 1:length(vars)){
        v = vars[i]
        axis_label = v
        # mixing diagram
        plot(0,0,xlim = range(m[,"ITER"]), ylim = range(m[,v]),type="n",xlab="",ylab="")
        title(main = "", xlab="", ylab = axis_label, cex.lab=text_size)
        for(c in unique(m[,"CHAIN"])){
            r = (m[,"CHAIN"] == c)
            lines(x=m[r,"ITER"], y=m[r,v], type='l', col = c)
        }
        if(i == length(vars)){
            title(xlab="Iterations", cex.lab=text_size)
        }
        #density plot
        dens = density(m[,v])
        plot(dens,xlab="",ylab="",main="")
        title(xlab=axis_label, ylab="Probability",main="", cex.lab=text_size)
    }
}