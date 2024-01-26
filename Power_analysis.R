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
