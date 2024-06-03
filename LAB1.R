library(TSstudio)
rainfall = c(799,114.8,865.1,1331.6,985.4,918.5,685.6,998.6,784.2,985,828.8,107.1)
rainfall.timeseries=ts(rainfall,start = c(2012,1),frequency = 12)
print(rainfall.timeseries)
plot(rainfall.timeseries)


#combined time series
rainfall1 = c(799,114.8,865.1,1331.6,985.4,918.5,685.6,998.6,784.2,985,828.8,107.1)
rainfall2 = c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)
combinedrain = matrix(c(rainfall1,rainfall2),nrow = 12)
rainfall.timeseries=ts(combinedrain,start = c(2012,1),frequency = 12)
print(rainfall.timeseries)
ts.plot(rainfall.timeseries,col = c("black","green"))

#working with air passenger data
data("AirPassengers")
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
plot(AirPassengers)

tsdata = ts(AirPassengers,frequency = 12)
ddata = decompose(tsdata,"multiplicative")
plot(ddata)

ts.plot(ddata$trend)
ts.plot(ddata$seasonal)
ts.plot(ddata$random)

plot(AirPassengers)
abline(reg = lm(AirPassengers~time(AirPassengers)))

boxplot(AirPassengers~cycle(AirPassengers),xlab = "date",ylab = "Passenger number(1000's)")

#working with nile data
ts.plot(Nile,xlab="year",ylab = "river volume(1e9m^3)")

#stationary and non-stationary time series
eps = rnorm(100,mean = 0,sd=1)
mu = 2
x_t = mu + eps
ts.plot(x_t,main = "Example of(random) stationary time series",ylab = "expression(x_t)")

#random walk process
z = rnorm(100,mean = 0.5,sd=1.5)
x =0 
for (i in 2:length(z)) {
  x[i]=x[i-1]+z[i]
}
ts.plot(x,main = "Raandom walk Process")