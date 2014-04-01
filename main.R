# install.packages("quantmod",repos="http://R-Forge.R-project.org")
# http://stackoverflow.com/questions/7525226/find-whether-a-particular-date-is-an-option-expiration-friday-problem-with-tim
rm(list=ls())
source('loadlibs.R')
source('functions.R')
source('names.R')

#UPDATE?
getnewdata <- F #TRUE
endcast <- Sys.Date()
backrizon = 240

# getnewdata()
# useolddata()

if(getnewdata){
  tic()
  getSymbols(calls, return.class='xts', verbose=TRUE, from=as.Date(endcast)-backrizon, to=endcast, src="yahoo")
  toc()
  save.image('stockdat.rdat')
}else{
  load('stockdat.rdat')
}
source('functions.R')
#save.image('stockdat.rdat')

#PARAMETERS
horizon = 30
emethod <-  "VAR" #"Univariate" #"RANDOMWALK" #"VAR"
forceARy <- FALSE
forceSAy <- FALSE
testyfit <- TRUE
facsig <- 0
desfacs <- 45 #45
maxsvd <- desfacs

names = calls2names(calls)
Y = get(names[1])[,6]
for (x in names[2:end(names)[1]]) {
  stock = get(x)[,6]
  Y = merge(Y,stock)
}

names(Y) <- names

Y <- Y[,complete.cases(t(Y))]

#svdbeta <- svd(Y,maxsvd)
svdbeta <- svd(Y)
d <- svdbeta$d
u <- svdbeta$u
v <- svdbeta$v

if (maxsvd <= 10){
  pairs(u)
} else {
#  pairs(u[,1:(maxsvd/2)])
#  pairs(u[,((maxsvd/2)+1):maxsvd])
}

ufactors <- timeSeries(u,time(Y[,1])) # if this fails: timeSeries(u,time(thisY))
names(ufactors)<- paste('F',1:maxsvd,sep='')
nobs <- dim(ufactors)[1]
allfacts <- ufactors

##################### SET STOCK HERE 
# DETERMINE WHICH STOCK WE'RE FORECASTING, WANT THIS IN LOOP
thisY <- Y$GE
# Y$AA Y$AMZN Y$GE Y$GM Y$F Y$C
Yval <- as.timeSeries(thisY)
##################### SET STOCK HERE

#svdfull <- svd(Y)
svdfull <- svdbeta
svdfull$u
fits <- lm(Yval~svdfull$u)

if(FALSE){
  sigfacs <- (abs(fits$coefficients)>=facsig)[-1]
  while(sum(sigfacs)>desfacs){
    sigfacs <- (abs(fits$coefficients)>=facsig)[-1]
    facsig <- facsig+0.0001
  }
}

sigfacs <- (abs(fits$effects[1:length(fits$coefficients)])>=facsig)[-1]
while(sum(sigfacs)>desfacs){
  sigfacs <- (abs(fits$effects[1:length(fits$coefficients)])>=facsig)[-1]
  facsig <- facsig+0.0001
}

print(paste("facsig threshold is:", facsig, "yielding",sum(sigfacs), "factors"))
#sort(abs(fits$coefficients), decreasing=TRUE)[1:10]
sum(sigfacs)
#svdfull$u[,sort(abs(fits$coefficients), decreasing=TRUE)[1:10]]
#svdfull$u[,svdfull$u %in% sort(abs(fits$coefficients), decreasing=TRUE)[1:10]]
selfacs <- svdfull$u[,sigfacs]
# str(sort(abs(fits$coefficients), decreasing=TRUE))
dumbnames <- names(sigfacs)[sigfacs]
lessdumbnames <- paste('Factor',gsub(".*u", "", dumbnames))
colnames(selfacs) <- lessdumbnames

# DETERMINE DEGREE OF DIFFERENCING FOR /THIS/ STOCK
# REQUIRED FOR THIS SECTION...
#   Yval, allfacts,REQUIRE AR?,REQUIRE SA?,testyfit
difference <- kpss.test(Yval)
kdif <- difference$parameter[[1]]
#kdif <- adf.test(Yval)$parameter[[1]]
# FIT FOR THIS STOCK
#if(FALSE){
fit2 <- auto.arima(Yval,
                   max.order=(20+kdif), 
                   d=kdif,
                   stepwise=FALSE,
#                   stepwise=TRUE,
#                   start.p=2,
#                   start.q=2,
#                   trace=TRUE,
#                   xreg=allfacts, 
                   xreg=selfacs,
                   allowdrift=TRUE,
                   parallel=TRUE,
                   num.cores=2, 
#                   test="adf", 
                   test="adf",
                   approximation=FALSE, 
                   ic="aicc",
                   lambda=BoxCox.lambda(Yval))
#}

#fit2 <- arima(Yval,order=c(2,kdif,2),xreg=allfacts)
# COLLECT ARIMA TERMS, ARIMA IT AGAIN: (P,Q,SAR,SMA,Period,dNS,dS)
arorder <- c(fit2$arma[1],fit2$arma[6],fit2$arma[2])
saorder <- c(fit2$arma[3],fit2$arma[7],fit2$arma[4])
speriod <- c(fit2$arma[5])

# Require at least one AR term
if(forceARy){
  arorder[1] <- ifelse(arorder[1]==0,1,arorder[1])
}

# Require 5 day seasonality
if(forceSAy){
  speriod <-5
  saorder[1] <- 1
  saorder[2] <- kdif
  saorder[3] <- 1
}

if(fit2$code!=1){
  print("Failed to converge. Trying harder.")
  fit2 <- arima(Yval,
              order=arorder,
              seasonal =list(order=saorder,period=speriod),
              xreg=selfacs,
              optim.control = c(maxit=50000))
  print(paste("Code is:", fit2$code))
}

if(testyfit){
# TEST RESIDUAL STATIONARITY
  adf.test(fit2$residuals)
#TEST RESIDUAL AUTOCORRELATION
  res = fit2$residuals 
  n = length(res) 
  mod2 = lm(res[-n] ~ res[-1]) 
  print(summary(mod2))
}

scaffold <- (nobs+1):(nobs+horizon)

# PROJECT THE EVOLUTION
newxregs <- makexregpath(emethod,scaffold,selfacs,horizon, nobs)
#newxregs2 <- makexregpathVAR(emethod,scaffold,selfacs,horizon, nobs)

# DO THE FORECAST
# Ideally, I'd like to be able to specify flexible levels and include ALL in plot
fore <- forecast(fit2, level=c(50,95), h=horizon, xreg=newxregs)

library(tis)
ftimes <- getfuturebdays(Yval, fore)
pred <- timeSeries(fore$mean, ftimes)
plotdat <- fcast2plottable(Yval, fore, ftimes)


title<-paste(trim(fore$method), "forecast of",names(Yval)[1]
            ,"at t =", horizon, "days, with evolution =",emethod)

f1 <- plot.fcast(plotdat, title)

print(fit2$loglik)
print(fit2$aic)

facfuture <- timeSeries(newxregs,ftimes)
#rbind(ufactors, facfuture)
#facall <- rbind(ufactors, facfuture)
facall <- rbind(selfacs, facfuture)

if (dim(facall)[2] <= 10){
  plot(facall) # FIND A BETTER WAY TO PRINT THIS
  Sys.sleep(3)
} else {
#  plot(facall[,1:(sum(sigfacs)/2)])
#  Sys.sleep(3)
#  plot(facall[,((sum(sigfacs)/2)+1):sum(sigfacs)])
#  Sys.sleep(3)
}

print(f1)
#print(pred[round(seq.int(from=1, to=horizon, length.out=5)),])
mydates <-c(time(pred[round(seq.int(from=1, to=horizon, length.out=5)),]))
print(plotdat[plotdat$date %in% as.Date(mydates),])

#ops <- getOptionChain(names(thisY),Exp=NULL)
ops <- readYahooOptions(names(thisY),Exp=NULL)
print(ops[[2]])
#next option date:
opfri1 <- timeNthNdayInMonth(timeFirstDayInMonth(Sys.Date()), nday=5, nth=3)
opfri2 <-timeNthNdayInMonth(timeFirstDayInMonth(Sys.Date()), nday=5, nth=3)
print(pred[opfri2,])