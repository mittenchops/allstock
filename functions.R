calls2names <- function(callvec){
  namevec <- gsub("[[:punct:]]","",callvec)
  return(namevec)
}

printarma <- function(fit2){
  #fit2$arma
  #(P,Q,SAR,SMA,Period,dNS,dS)
  arorder <- c(fit2$arma[1],fit2$arma[6],fit2$arma[2])
  saorder <- c(fit2$arma[3],fit2$arma[7],fit2$arma[4])
  speriod <- c(fit2$arma[5])
  print(paste("AR order is: (", arorder[1],",",arorder[2],",",arorder[3],")","(",saorder[1],",",saorder[2],",",saorder[3],"), s=",speriod, sep=""))
  #  print(paste("Seasonal period is: ",unlist(speriod), sep=""))
}

makexregpathVAR <- function(emethod,scaffold,infacs,horizon,nobs){
  print(paste('Evolution method is ',emethod,sep=''))
  varfit <- VAR(infacs, type="none", lag.max=1, ic="AIC")
  evolution <- predict(varfit, n.ahead=horizon)
#  dumbu <- array(0,dim=c(horizon))
#  for (x in seq(1, dim(infacs)[2])){
#    fac <- evolution$fcst[[x]][,1]
#    dumbu <- cbind(dumbu,fac)
#  }
#  dumbu <- dumbu[,-1]
#  newxregs <- data.frame(dumbu)
  newxregs <- evolution
#  print(evolution)
  }

makexregpath <- function(emethod,scaffold,infacs,horizon,nobs){
  print(paste('Evolution method is ',emethod,sep=''))
  if(emethod=="VAR") {
    varfit <- VAR(infacs, type="none", lag.max=1, ic="AIC")
#    restrict <- matrix(diag(dim(infacs)[2]),
#                       nrow=dim(infacs)[2], ncol=dim(infacs)[2], byrow=TRUE)
#    restrict(varfit, method = "man", resmat = restrict)
#    restrict(varfit, method = "ser")
#   cajo <- ca.jo(infacs)
    evolution <- predict(varfit, n.ahead=horizon)
    dumbu <- array(0,dim=c(horizon))
    for (x in seq(1, dim(infacs)[2])){
      fac <- evolution$fcst[[x]][,1]
      dumbu <- cbind(dumbu,fac)
    }
    dumbu <- dumbu[,-1]
    newxregs <- data.frame(dumbu)
    print(evolution)
  } else {
  dumbu <- array(0,dim=c(horizon))
  for (x in seq(1, dim(infacs)[2])){
#    difference <- adf.test(infacs[,x])
#    kdif <- difference$parameter[[1]]
    eu <- auto.arima(infacs[,x], max.p=1000, max.q=1000, max.order=10000,ic="aicc")
    arorder <- c(eu$arma[1],eu$arma[6],eu$arma[2])
    if(eu$code!=0){
      print(paste("Factor", x, "failed to converge."))
      eu <- arima(infacs[,x], order=arorder, optim.control = c(maxit=80000), optim.method="L-BFGS-B")
      print(paste("First code status for this factor was ",eu$code))
      if(eu$code!=0){print("STOP THE PRESSES!!!")}
      }
    fac <- forecast(eu,h=horizon)$mean
#    names(fac) <- colnames(infacs)[x]
    print(paste("Now factor number",x))
    print(paste(colnames(infacs)[x], " fit:", sep=""))
    printarma(eu)
    dumbu <- cbind(dumbu,fac)
    }
  dumbu <- dumbu[,-1]
  newxregs <- data.frame(dumbu)
  }
  names(newxregs)[1:dim(newxregs)[2]] <- colnames(infacs)
  print('EXECUTED')
  return(newxregs)
}

tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

toc <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}

getfuturebdays <- function(Yval, fore){
  require(tis)
  firstfcastdate <- last(time(Yval))+60*60*24
  bforecast <- tis(fore$mean, as.character(firstfcastdate), frequency=262)
  ftimes <- ti(bforecast)
  return(ftimes)
}

fcast2plottable <- function(Yval, fore, ftimes){
  # COLLECT THE PARTS YOU WANT TO GRAPH
  tu <- fore$upper[,1] # tight, upper
  lu <- fore$upper[,2] # loose, upper
  td <- fore$lower[,1]
  ld <- fore$lower[,2]
  
  # MAP FORECASTS ONTO BUSINESS DATE VECTOR
  pred <- timeSeries(fore$mean, ftimes)
  uconf <- timeSeries(lu, ftimes)
  lconf <- timeSeries(ld, ftimes)
  u50conf <- timeSeries(tu, ftimes)
  l50conf <- timeSeries(td, ftimes)
  
  yvars = merge(Yval,pred)[,1]  # total time vector, beginning to end?
  yvars = cbind(Yval, pred)     # PAST SERIES
  yvars = cbind(yvars, lconf)   # Lower
  yvars = cbind(yvars, uconf)   # Upper
  yvars = cbind(yvars, u50conf) # Upper
  yvars = cbind(yvars, l50conf) # Lower
  date = time(yvars)            # Date
  
  plotdat <- data.frame(
    Series = as.numeric(yvars[,1]),
    Forecast = as.numeric(yvars[,2]),
    LowerBound = as.numeric(yvars[,3]),
    UpperBound = as.numeric(yvars[,4]),
    U50Bound = as.numeric(yvars[,5]),
    L50Bound = as.numeric(yvars[,6]),
    date = as.Date(strptime(date, format="%Y-%m-%d")) )
  
  sfc <- nrow(Yval) # painfully necessary
  
  #if(x == 1) {
  print('Linking forecasts to Horizon')
  print(plotdat[sfc,])
  #}
  plotdat$Forecast[sfc] = plotdat$Series[sfc]   #FIX THIS
  plotdat$LowerBound[sfc] = plotdat$Series[sfc]  #FIX THIS
  plotdat$UpperBound[sfc] = plotdat$Series[sfc]	#FIX THIS
  plotdat$U50Bound[sfc] = plotdat$Series[sfc]	#FIX THIS
  plotdat$L50Bound[sfc] = plotdat$Series[sfc]	#FIX THIS
  #if(x == 1) {
  print(plotdat[sfc,])
  #}
  return(plotdat)
}

plot.fcast <- function(plotdat, title){
opfri <- as.Date(timeNthNdayInMonth(timeFirstDayInMonth(Sys.Date()+27), nday=5, nth=3),format="%Y-%m-%d")
  f1 <- ggplot(plotdat, aes(x=date) ) +
    geom_line(aes(y = Series), size=1.1) +
    theme_bw() +
    ylab("Price") +
    xlab("Month") +
    ylim(max(min(min(plotdat$LowerBound,na.rm=TRUE), min(plotdat$Series,na.rm=TRUE)),0, na.rm=TRUE),
         max(min(max(max(plotdat$UpperBound,na.rm=TRUE), max(plotdat$Series,na.rm=TRUE)), max(plotdat$Forecast*1.1, na.rm=TRUE), na.rm=TRUE),max(plotdat$Series,na.rm=TRUE))) +
           geom_smooth(aes(y = Forecast, ymin = LowerBound, ymax = UpperBound), stat="identity", alpha=0.05) +
           geom_smooth(aes(y = Forecast, ymin = L50Bound, ymax = U50Bound), stat="identity", alpha=0.25) +
           geom_line(aes(y = Forecast), colour="red", size=0.90) +
           geom_line(aes(y = LowerBound), colour="red", size=0.25, linetype="longdash") +
           geom_line(aes(y = UpperBound), colour="red", size=0.25, linetype="longdash") +
           geom_vline(xintercept=as.numeric(plotdat$date[plotdat$date==opfri]), linetype="dotted") +
           scale_x_date(breaks = "1 month", minor_breaks = "1 week", labels=date_format("%B")) +
           opts(legend.position = "none", title=title, axis.title.x = theme_text(vjust = 0))
  return(f1)
}

readYahooOptions <- function(Symbols, Exp, ...){
  parse.expiry <- function(x) {
    if(is.null(x))
      return(NULL)
    
    if(inherits(x, "Date") || inherits(x, "POSIXt"))
      return(format(x, "%Y-%m"))
    
    if (nchar(x) == 5L) {
      x <- sprintf(substring(x, 4, 5), match(substring(x,
                                                       1, 3),
                                             month.abb), fmt = "20%s-%02i")
    }
    else if (nchar(x) == 6L) {
      x <- paste(substring(x, 1, 4), substring(x, 5, 6),
                 sep = "-")
    }
    return(x)
  }
  
  clean.opt.table <- function(tableIn){
    tableOut <- lapply(tableIn[,-2], function(x) as.numeric(gsub(",","",x)))
    rownames(tableOut) <- tableIn[,2]
  }
  
  if(missing(Exp))
    optURL <- paste(paste("http://finance.yahoo.com/q/op?s",Symbols,sep="="),"Options",sep="+")
  else
    optURL <- paste(paste("http://finance.yahoo.com/q/op?s=",Symbols,"&m=",parse.expiry(Exp),sep=""),"Options",sep="+")
  
  if(!missing(Exp) && is.null(Exp)) {
    optPage <- readLines(optURL)
    optPage <- optPage[grep("View By Expiration", optPage)]
    allExp <- gregexpr("m=", optPage)[[1]][-1] + 2
    allExp <- substring(optPage, allExp, allExp + 6)
    allExp <- allExp[seq_len(length(allExp)-1)] # Last one seems useless ? Always true?
    return(structure(lapply(allExp, readYahooOptions,
                            Symbols=Symbols), .Names=format(as.yearmon(allExp))))
  }
  
  stopifnot(require("XML"))
  
  optURL <- readHTMLTable(optURL)
  
  # Not smart to hard code these but it's a 'good-enough' hack for now
  # Also, what is table 9 on this page?
  CALLS <- optURL[[10]]
  PUTS <- optURL[[14]]
  
  list(calls = CALLS, puts = PUTS, symbol = Symbols)
}