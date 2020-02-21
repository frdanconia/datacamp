
iliadis.plot = function(mcmcList,x,y){
    dev.new()
    num_lines = 500
    xlim = range(x)
    ylim = range(y)
    plot(x,y, xlim=xlim, ylim=ylim)

    all_samples = as.matrix(mcmcList)
    samples = all_samples[round(seq(from = 1, to = nrow(all_samples), length = num_lines)),]
    slopes = samples[,"b"]
    intercepts = samples[,"a"]

    ## regression line distribution (grey lines).
    for(i in 1:nrow(samples)){
        abline(intercepts[i],slopes[i],col=adjustcolor("black",alpha = 0.01))
    }

    ## credible region (red,blue lines)
    cred = list()
    cred$x = seq(xlim[1], xlim[2], length=num_lines)
    for(i in 1:length(cred$x)){
        cred$lower[i] = quantile(intercepts + slopes * cred$x[i], prob = 0.025)
        cred$middle[i]= quantile(intercepts + slopes * cred$x[i], prob = 0.50)
        cred$upper[i] = quantile(intercepts + slopes * cred$x[i], prob = 0.975)
    }
    lines(cred$x, cred$lower, col='red')
    lines(cred$x, cred$upper, col='red')
    lines(cred$x, cred$middle, col='blue')

    ## mean intrinsic scatter lines
    m = mean(samples[,"sigma"])
    lines(cred$x, cred$middle + 2*m, lty = 2, col = 'black')
    lines(cred$x, cred$middle - 2*m, lty = 2, col = 'black')
}