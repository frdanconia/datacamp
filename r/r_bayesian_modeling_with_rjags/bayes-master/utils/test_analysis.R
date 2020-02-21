test.analysis = function(N = 50){

    library(rjags)

    true_slope = 1
    true_intercept = -5
    true_sigma = 2
    x = runif(N,0,10)
    y = rnorm(N,true_slope * x + true_intercept, true_sigma)

    model_string = "
    model {
        ## priors:
        b ~ dunif(-10,10)
        a ~ dunif(0,10)
        sigma ~ dunif(0,5)

        ## structure:
        for (i in 1:N) {
        y[i] ~ dnorm(b * x[i] + a, pow(sigma, -2))
        }
    }
    "

    model = jags.model(file = textConnection(model_string),
                       data = list('x' = x,
                       'y' = y,
                       'N' = N),
                       n.chains = 1,
                       n.adapt = 1000)

    output = coda.samples(model = model,
                          variable.names = c("a", "b", "sigma"),
                          n.iter=1000,
                          thin=1)
    return(list(mcmcList = output, x = x, y = y))
}