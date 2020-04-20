# tsa4 - Shumway Stoffer
# p. 24, 25

# required libraries
library(astsa)

# simulate data
x = rnorm(100)
y = lag(x, -5) + rnorm(100)

# plot the data
par(mfrow = c(2, 1))
plot(1:100, x, type = "l", main = "series x")
abline(h = 0)
plot(1:100, y, type = "l", main = "series y")
abline(h = 0)

# the cross-covariance function will look like the autocovariance of the input
# series x_{t} , and it will have a peak on the positive side if x_{t} leads y_{t} 
# and a peak on the negative side if x_{t} lags y_{t}

# plot cross-covariance function
par(mfrow = c(1, 1))
acf(y, type = 'covariance')
acf(x, type = 'covariance')
ccf(y, x, ylab = 'CCovF', type = 'covariance')

# plot cross-correlation function
ccf(y, x, ylab = 'CCF')
ccf2(y, x, ylab = 'CCF', max.lag = 16)

# x leads y in the positive part of horizontal axis