library(ggplot2)
library(cowplot)
library(openxlsx)
library(qqplotr)
library(scales)
library(grid)
library(anytime)
library(lubridate) 
library(tseries) 
library(timeSeries)
library(forecast)
library(tictoc)
library(xts)
library(quantmod)
library(rugarch)
library(readxl)
library(nortest)
## fitted data
# Selected output from code in ru_AR1gjrArch1fitFTSE.txt
# Read in data frame with returns and dates, etc
FTSEdata <- read.xlsx("FTSE100dataset2005-11.xlsx", sheet=1)
FTSEdata$Date <- as.Date(FTSEdata$Date, origin="1899-12-30")
str(FTSEdata)
# Building the model
varianceModel=list(model='gjrGARCH', garchOrder=c(1,0))
meanModel=list(armaOrder=c(1,0))
distributionModel='norm'
fixedPars=list(ma1=0)
spec <- ugarchspec(variance.model=varianceModel, distribution.model=distributionModel,
                   fixed.pars=fixedPars)
gjrGarch.fit <- ugarchfit(spec, data=FTSEdata$serReturn, solver='hybrid')
#str(gjrGarch.fit) # useful to begin but very detailed
show(gjrGarch.fit) # gives main tables of info below

## forcasting
spec1 = ugarchspec(variance.model=list(model="csGARCH"), distribution="std")
fit = ugarchfit(spec1, FTSEreturn)
bootp = ugarchboot(fit, method = c("Partial", "Full")[1],
                   n.ahead = 500, n.bootpred = 500)
show(bootp)
plot(bootp,wich=2)

## rolling estimation
cl = makePSOCKcluster(10)
spec = ugarchspec(variance.model = list(model = "eGARCH"), distribution.model = "jsu")
roll = ugarchroll(spec,FTSEreturn, n.start = 200, refit.every = 20,
                    refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                    VaR.alpha = c(0.01, 0.05), cluster = cl, keep.coef = TRUE)
show(roll)
stopCluster(cl)
report(roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)
plot(roll)


## rolling estimation with plot
FTSEreturn <- read.xlsx("FTSEreturn.xlsx", sheet=1)
spec = ugarchspec()
fit = ugarchfit(spec, FTSEreturn, out.sample=10)
forc = ugarchforecast(fit, n.ahead = 25, n.roll = 10)
f = fitted(forc)
# this is a 25 x 11 matrix [n.ahead x (n.roll+1)]
# colnames: T+0 date index
T0 = as.POSIXct(colnames(f))
rollT1 = move(T0, by=1)
# rolling estimation
plot(xts(f["T+1",],rollT1))



