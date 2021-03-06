# Kevin O'Connor
# STAT261 Final Project

# Reading in data.
weather_data = read.table("/Users/kevinoconnor/Documents/School/STAT261/FinalProject/weather_data.txt", head=T)
raw_data = weather_data
years = weather_data[,1]
weather_data = stack(data.frame(rbind(t(weather_data)[2,], t(weather_data)[3,], t(weather_data)[4,], t(weather_data)[5,])))$values

# Clipping unknown values from data.
weather_data = weather_data[-1]
weather_data = weather_data[-length(weather_data)]
weather_data = weather_data[-length(weather_data)]
weather_data = weather_data[-length(weather_data)]

# Creating time series object.
weather_data = ts(weather_data, start=c(years[1], 2), end=c(tail(years,1), 1), frequency=4)

# Plotting data.
pdf("STAT261FinalProjectPlot1.pdf", width=13, height=5)
plot(weather_data, main="Quarterly Average Temperature", ylab="Ave. Temp. (C)")
dev.off()

# Plotting acf.
pdf("STAT261FinalProjectACF.pdf", width=13, height=5)
acf(weather_data, main="Quarterly Average Temperature ACF")
dev.off()

# Plotting pacf.
pdf("STAT261FinalProjectPACF.pdf", width=13, height=5)
pacf(weather_data, main="Quarterly Average Temperature PACF")
dev.off()

# Summary statistics.
max(weather_data) # Max temperature: 17.8
years[which(weather_data == max(weather_data))/4]+1 # Year of max temperature: 1976
min(weather_data) # Min temperature: -1.2
years[which(weather_data == min(weather_data))/4]+1 # Year of min temperature: 1684
mean(weather_data) # Average overall temperature: 9.230
mean(raw_data$DJF[-1]) # Average temperature from Dec-Feb: 3.742
mean(raw_data$MAM[-length(raw_data[,1])]) # Average temperature from Mar-May: 8.162
mean(raw_data$JJA[-length(raw_data[,1])]) # Average temperature from June-Aug: 15.310
mean(raw_data$SON[-length(raw_data[,1])]) # Average temperature from Sep-Nov: 9.706
var(weather_data) # Variance in temperature: 18.150
var(raw_data$DJF[-1]) # Variance from Dec-Feb: 1.861
var(raw_data$MAM[-length(raw_data[,1])]) # Variance from Mar-May: 0.787
var(raw_data$JJA[-length(raw_data[,1])]) # Variance from June-Aug: 0.658
var(raw_data$SON[-length(raw_data[,1])]) # Variance from Sep-Nov: 0.811

# Fitting linear trend model.
lin_trend_fit = lm(weather_data ~ time(weather_data))
summary(lin_trend_fit)
confint(lin_trend_fit)
pdf("STAT261FinalProjectPlot2.pdf", width=13, height=5)
plot(time(weather_data), lin_trend_fit$res, main="Linear Trend Fit Residuals", ylab="Residual", xlab="Time")
dev.off()

# Detrending based on linear trend model.
detrended_weather = weather_data - fitted.values(lin_trend_fit)

# Finding periodogram.  
I = abs(fft(detrended_weather))^2/length(detrended_weather)
P = 4/length(detrended_weather)*I[1:(length(detrended_weather)/2)]
f = (0:(length(detrended_weather)/2 - 1))/length(detrended_weather)
pdf("STAT261FinalProjectPeriodogram.pdf", width=13, height=5)
plot(f, P, main="Scaled Periodogram", xlab="Frequency", ylab="Scaled Periodogram", type="l")
dev.off()

# Estimating seasonal component using iterated loess fitting.
decomp = stl(weather_data, "periodic")
unseasoned = weather_data - decomp$time.series[,1]
lin_trend_fit2 = lm(unseasoned ~ time(weather_data))
summary(lin_trend_fit2)
confint(lin_trend_fit2)

# Forward step-wise model selection on trend.  
sq = time(weather_data)^2
quad_trend_fit = lm(unseasoned ~ time(weather_data) + sq)
anova(lin_trend_fit2, quad_trend_fit, test="Chisq")
cu = time(weather_data)^3
cubic_trend_fit = lm(unseasoned ~ time(weather_data) + sq + cu)
anova(quad_trend_fit, cubic_trend_fit, test="Chisq")
qu = time(weather_data)^4
quartic_trend_fit = lm(unseasoned ~ time(weather_data) + sq + cu + qu)
anova(cubic_trend_fit, quartic_trend_fit, test="Chisq")

# Examining residuals. 
pdf("STAT261FinalProjectPlot3.pdf", width=13, height=5)
plot(time(weather_data), cubic_trend_fit$res, main="Cubic Trend Fit Residuals", ylab="Residual", xlab="Time")
abline(h=0)
dev.off()

# Plotting unseasoned data with cubic trend fit.  
pdf("STAT261FinalProjectPlot4.pdf", width=13, height=5)
plot(time(weather_data), unseasoned, main="Unseasoned Quarterly Weather Data with Fit", ylab="Temperature", xlab="Time", cex=0.25, type="l")
points(time(weather_data), fitted.values(cubic_trend_fit), type="l", lwd=5)
dev.off()

# Fitting data to an arma model.  
det_unseasoned = unseasoned - fitted.values(cubic_trend_fit)
arma_fit = sarima(det_unseasoned, p=2, d=0, q=1)
arma_fit

# Forecasting 10 years into future.
pdf("STAT261FinalProjectPlot5.pdf", width=13, height=5)
sarima.for(det_unseasoned, n.ahead=40, p=2, d=0, q=1)
dev.off()