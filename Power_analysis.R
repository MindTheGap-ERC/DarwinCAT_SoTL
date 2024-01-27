require(MKpower)
#Functions that work with MKpower::sim.ssize.wilcox.test, 
# the test needs to call a function sampling from a distribution
rx <- function(n) rnorm(n, mean = 5, sd = 1) 
ry <- function(n) rnorm(n, mean = 7, sd = 1) 

#DIY discrete distribution from rnorm: this object is a dummy to test it works
mean6 <- hist(rnorm(50, 
                    mean=6/9, 
                    sd = 1), 
              breaks = 9)

#If you call:
sample6 <- sample(x=mean6$breaks[1:10], 
                  size=1000, 
                  replace=TRUE, 
                  prob=mean6$density)
# It works

function sample_discrete(mean, size, sd, breaks) {
  mean_hist <- hist(rnorm(x = size,
                          mean = mean,
                          sd = sd),
                    breaks = breaks)
  sampled_discrete <- sample(x=mean_hist$breaks[1:length(mean5$breaks)-1], 
                size=size, 
                replace=TRUE, 
                prob=mean_hist$density)
  return sampled_discrete
}

# This is the test we want to use but we need to call a function sampling
# from a distribution: rx and ry
MKpower::sim.ssize.wilcox.test(rx = ry, 
                               n.max = 100, 
                               iter = 1000, 
                               type = "one.sample")

bin_norm = function(mean, sd, breaks, normalize = TRUE){
  #' @title discretize normal distribution
  #' 
  #' @param mean numeric, mean of normal distribution
  #' @param sd strictly positive numeric, standard deviation of normal distribution
  #' @param breaks numeric vector with strictly increasing entries, bins used for discretization
  #' @param normalize logical, should weights be normalized to add up to 1?
  #' 
  #' @returns numeric vector of length length(breaks) - 1, containing probabilities that the normal distribution is within breaks[i] and breaks[i+1]
  #' 
  weights = diff(pnorm(breaks, mean = mean, sd = sd))
  if(normalize){
    weights = weights / sum(weights)
  } 
  return(weights)
}

rdisc_distrib = function(n, weights, vals){
  #' @title simulate disc. distribution
  #' 
  #' @param n sample size
  #' @param weights probabilities to draw a value. weights[i] is the probability to draw vals[i]
  #' @param vals values to draw from
  #' 
  #' @returns vector of length n with samples drawn from vals according to weights
  #' 
  
  s = runif(n, min = 0, max = 1)
  cum_weights = cumsum(weights)
  samp = sapply(seq_len(n), function(i) vals[which.max(s[i] < cum_weights)] )
  return(samp)
}


sim_disc_normal = function(n, mean, sd, breaks, vals){
  #' @title simulate discretized normal distribution
  #' 
  #' @param n sample size
  #' @param mean mean
  #' @param sd standard deviation
  #' @param breaks breaks for discretization
  #' @param vals values to draw from
  
  weights = bin_norm(mean = mean, sd = sd, breaks = breaks, normalize = TRUE)
  sa = rdisc_distrib(n = n, weights = weights, vals = vals)
  return(sa)
}

# function for the power test
rx = function(n) sim_disc_normal(n, mean = 6.5, sd = 2, breaks = seq(-0.5,9.5, 1), vals  = 0:9)
hist(rx(10000))
