# tsa4 - Shumway Stoffer
# p. 30

# required libraries
library(astsa)

# Southern Oscillation Index (SOI) and associated Recruitment (number of new fish) example
par(mfrow = c(2, 1))
plot(soi, ylab = "", xlab = "", main = "Southern Oscillation Index")
plot(rec, ylab = "", xlab = "", main = "Recruitment")


# acf and ccf -------------------------------------------------------------
par(mfrow = c(3, 1))
acf(soi, 48, main = "Southern Oscillation Index")
acf(rec, 48, main = "Recruitment")
ccf(soi, rec, 48, main = "SOI vs Recruitment", ylab = "CCF")


# We could say the SOI leads the Recruitment series by six months. The
# sign of the CCF is negative, leading to the conclusion that the two 
# series move in different directions; that is, increases in SOI lead to
# decreases in Recruitment and vice versa


# prewhitening a series prior to a cross-correlation analysis -------------
# simulate two series
set.seed(1492)
num = 120 
t = 1:num
X = ts(2 * cos(2 * pi * t / 12) + rnorm(num), freq = 12)
Y = ts(2 * cos(2 * pi * (t + 5) / 12) + rnorm(num), freq = 12)

# prewhitening Y
Yw = resid(lm(Y ~ cos(2 * pi * t / 12) + sin(2 * pi * t / 12), na.action = NULL) )

# plot
par(mfrow = c(3, 2), mgp = c(1.6, .6, 0), mar = c(3, 3, 1, 1))
plot(X)
plot(Y)
acf(X, 48, ylab = 'ACF(X)')
acf(Y, 48, ylab = 'ACF(Y)')
ccf(X, Y, 24, ylab = 'CCF(X,Y)')
ccf(X, Yw, 24, ylab = 'CCF(X,Yw)', ylim = c(-.6, .6))


# scatterplot matrices ----------------------------------------------------
# fig. 2.8 p. 63
lag1.plot(soi, 12)
# lowess fits are approximately linear, so the sample autocorrelations are meaningful

# fig. 2.9 p. 64
lag2.plot(soi, rec, 8)
# fig. 2.9 shows a fairly strong nonlinear relationship between Recruitment,R_{t},
# and the SOI series at S_{t−5}, S_{t−6}, S_{t−7}, S_{t−8}


# preliminary analysis of the recruitment series --------------------------
# example 3.18 p. 101
acf2(rec, 48)
(regr = ar.ols(rec, order = 2, demean = FALSE, intercept = TRUE))
regr$asy.se.coef # standard errors of the estimates


# relating the prewhitened soi to the transformed recruitment series
# example 5.8 p. 267
soi_d = resid(lm(soi ~ time(soi), na.action = NULL)) # detrended SOI
acf2(soi_d)

fit = arima(soi_d, order = c(1, 0, 0))
ar1 = as.numeric(coef(fit)[1]) # = 0.5875

soi_pw = resid(fit)
rec_fil = filter(rec, filter = c(1, -ar1), sides = 1)

ccf(soi_pw, rec_fil, ylab = "CCF", na.action = na.omit, panel.first = grid())