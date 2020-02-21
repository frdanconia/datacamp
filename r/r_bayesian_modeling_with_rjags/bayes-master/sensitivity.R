source("utils.R")

N = 50
true_slope = 1
true_intercept = 5
true_sigma = 2
x = runif(N,0,10)
y = rnorm(N,true_slope * x + true_intercept, true_sigma)

model_template = "
model {
    ## priors, to be filled in later:
    b ~ %s
    a ~ %s
    sigma ~ %s

    ## structure:
    for (i in 1:N) {
    y[i] ~ dnorm(a + b * x[i], pow(sigma, -2))
    }
}
"

# priors to be used for a,b,sigma -- all permutations will be calculated
# note: distributions that allow negative numbers will mess up sigma
priors = c(
"dunif(0,10)",
"dunif(0,100)",
"dlnorm(0,1)",
"dlnorm(0,1/100)",
"dlnorm(0,1/10000)"
)

# set up the plots
usr_list = list()
par(mfrow = c(3,1))
# slope:
plot(x=c(0,2), y=c(0,5), ylab="density", xlab="b", type="n")
usr_list[[1]] = par("usr")

# intercept:
plot(x=c(0,10), y=c(0,1), ylab="density", xlab="a", type="n")
usr_list[[2]] = par("usr")

# sigma:
plot(x=c(0,4), y=c(0,3), ylab="density", xlab="sigma", type="n")
usr_list[[3]] = par("usr")

# stupid way to generate permutations WITH replacement
# requires the combinat package
combs = unique(combinat::combn(rep(priors,3),3,simplify=F))

for(comb in combs){
    model_string = sprintf(model_template, comb[1], comb[2], comb[3])
    # specify inits so that only the effect of the prior is seen
    # the capture.output absorbs what these functions print to the console
    garbage = capture.output(model <- jags.model(file = textConnection(model_string),
                       data = list('x' = x,
                       'y' = y,
                       'N' = N),
                       n.chains = 1,
                       n.adapt = 1000,
                       inits = list('.RNG.name' = 'base::Mersenne-Twister', 
                                    '.RNG.seed' = 1)))

    garbage = capture.output(output <- coda.samples(model = model,
                          variable.names = c("b", "a", "sigma"),
                          n.iter=1000,
                          thin=1))
    chain = output[[1]]
    median = c()
    name = c("b","a","sigma")
    for(i in 1:3){
        median[i] = quantile(chain[,name[i]], prob=0.50)
        par(mfg = c(i,1))
        par(usr = usr_list[[i]])
        lines(density(chain[,name[i]]))
    }

    cat("with b ~", comb[1], ", a ~", comb[2], ", sigma ~", comb[3], "\n")
    cat("median(b)     =", median[1], "\n")
    cat("median(a)     =", median[2], "\n")
    cat("median(sigma) =", median[3], "\n")
    cat("\n")
}
