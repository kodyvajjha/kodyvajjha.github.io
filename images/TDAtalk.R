library(TDA)
library(rgl)
library(crypto)
library(quantmod)
library(pracma)

######################################################################
#################### STOCK MARKET TDA ANALYSIS #######################
######################################################################

getSymbols(c("^GSPC","^DJI","^IXIC","^RUT"))

multits <- cbind(diff(log(GSPC[,6])),diff(log(DJI[,6])),diff(log(IXIC[,6])),diff(log(RUT[,6])))

w=100
tseq <- seq(from = 0, to = 5, length = 1000)

lnorms <- function(n){
  X <- multits[seq(n,w+n-1),]
  Y <- ripsDiag(X, maxdimension = 1, maxscale = 5)
  land <- landscape(Diag=Y[["diagram"]],dimension=1,KK=1,tseq=tseq)
  t <- c((Norm(land,p=1)),(Norm(land,p=2)))
    return(t)
}

L <- sapply(seq(1,length(multits[,1])+1-w),lnorms) #actually works!
L1.app <- append(t(L[1,]),rep(0,2865-2766))
L2.app <- append(t(L[2,]),rep(0,2865-2766))

retnorms<-cbind(multits,L1.app,L2.app)
plot(retnorms[,1],main="Returns of S&P 500")
plot(retnorms[,5],main="L1 norm")
plot(retnorms[,6],main="L2 norm")

L.time <- rbind(index(multits))
par(mfrow=c(3,1))
plot(multits[,1], main = "Returns of S&P 500, DJIA, NASDAQ, RUT",type='l')
plot(L[1,],type='l', main = "L1 norms of landscapes")
plot(L[2,],type='l')

######################################################################
################## CRYPTOCURRENCY TDA ANALYSIS #######################
######################################################################

BTC <- getCoins(coin = c("BTC"))
ETH <- getCoins(coin = c("ETH"))
XRP <- getCoins(coin = c("XRP"))
BCH <- getCoins(coin = c("BCH"))

BTC.xts <- xts(BTC[,9],order.by = as.Date(BTC[,c(4)],"%Y-%m-%d"))
ETH.xts <- xts(ETH[,9],order.by = as.Date(ETH[,c(4)],"%Y-%m-%d"))
XRP.xts <- xts(XRP[,9],order.by = as.Date(XRP[,c(4)],"%Y-%m-%d"))
BCH.xts <- xts(BCH[,9],order.by = as.Date(BCH[,c(4)],"%Y-%m-%d"))

multits.crypto <- na.omit(cbind(diff(log(BTC.xts)),diff(log(ETH.xts)),diff(log(XRP.xts)),diff(log(BCH.xts))))

plot(multits.crypto)

chart_Series(multits.crypto)

v=14

lnorms.crypto <- function(n){
  X <- multits.crypto[seq(n,v+n-1),]
  #print(X)
  Y <- ripsDiag(X, maxdimension = 1, maxscale = 5)
  land <- landscape(Diag=Y[["diagram"]],dimension=1,KK=1,tseq=tseq)
  t <- c((Norm(land,p=1)),(Norm(land,p=2)))
  return(t)
}

L.crypto <- sapply(seq(1,length(multits.crypto[,1])+1-v),lnorms.crypto)
L1.app.crypto <- append(t(L.crypto[1,]),rep(0,length(multits.crypto[,1])-length(L.crypto[1,])))
L2.app.crypto <- append(t(L.crypto[2,]),rep(0,length(multits.crypto[,1])-length(L.crypto[1,])))

retnorms.crypto<-cbind(multits.crypto,L1.app.crypto,L2.app.crypto)
par(mfrow=c(3,1))
plot(BCH.xts)
plot(retnorms.crypto[,4],main="Returns of Bitcoin Cash")
plot(retnorms.crypto[,5],main="L1 norm")
plot(retnorms.crypto[,6],main="L2 norm")

bitL1 <- cbind(BTC.xts,retnorms.crypto[,5],retnorms.crypto[,6])

L.crypto.time <- rbind(BCH[,4],L.crypto)
#L.crypto.xts <- xts(t(L.crypto), order.by = as.Date(BCH[-seq(1,14),c(4)],"%Y-%m-%d"))
L.crypto.xts <-xts(t(L.crypto), order.by = index(multits.crypto[-seq(1,v-1)]),"%Y-%m-%d")

par(mfrow=c(3,1))
chart_Series(BCH.xts)
chart_Series(multits.crypto[,1],title = "Hello")
chart_Series(L.crypto.xts)
chartSeries(L.crypto.xts,theme=chartTheme('white'))

# Finding the Persistence diagram on the date of highest L1/2 norm.
max(L.crypto.xts)

######################################################################
################## STOCK HIGH FREQUENCY TDA ANALYSIS #################
######################################################################

setwd('/home/kody/Scripts/R/TDA/Data/')

cipla <- read.csv('CIPLA-10min.csv')
cipla$Time <- substr(as.character(cipla$date),12,19)
cipla$date <- substr(as.character(cipla$date),1,10)
cipla.zoo <- read.zoo(cipla,index=c(1,7),tz="",format = "%Y-%m-%d %H:%M:%S")
cipla.xts<-xts(cipla.zoo[,3])

indigo <- read.csv('INDIGO-10min.csv')
indigo$Time <- substr(as.character(indigo$date),12,19)
indigo$date <- substr(as.character(indigo$date),1,10)
indigo.zoo <- read.zoo(indigo,index=c(1,7),tz="",format = "%Y-%m-%d %H:%M:%S")
indigo.xts<-xts(indigo.zoo[,3])

reliance <- read.csv('RELIANCE-10min.csv')
reliance$Time <- substr(as.character(reliance$date),12,19)
reliance$date <- substr(as.character(reliance$date),1,10)
reliance.zoo <- read.zoo(reliance,index=c(1,7),tz="",format = "%Y-%m-%d %H:%M:%S")
reliance.xts<-xts(reliance.zoo[,3])

spicejet <- read.csv('SPICEJET-10min.csv')
spicejet$Time <- substr(as.character(spicejet$date),12,19)
spicejet$date <- substr(as.character(spicejet$date),1,10)
spicejet.zoo <- read.zoo(spicejet,index=c(1,7),tz="",format = "%Y-%m-%d %H:%M:%S")
spicejet.xts<-xts(spicejet.zoo[,3])

tatasteel <- read.csv('TATASTEEL-10min.csv')
tatasteel$Time <- substr(as.character(tatasteel$date),12,19)
tatasteel$date <- substr(as.character(tatasteel$date),1,10)
tatasteel.zoo <- read.zoo(tatasteel,index=c(1,7),tz="",format = "%Y-%m-%d %H:%M:%S")
tatasteel.xts<-xts(tatasteel.zoo[,3])

par(mfrow=c(3,1))
chart_Series(tatasteel.xts)
chart_Series(cipla.xts)
chart_Series(indigo.xts)

multits.highfreq <-na.omit(cbind(diff(log(cipla.xts)),
                                 diff(log(indigo.xts)),
                                 diff(log(tatasteel.xts)),
                                 diff(log(spicejet.xts)),
                                 diff(log(reliance.xts)))
                           )
chart_Series(multits.highfreq[,2])
chart_Series(multits.highfreq[,4])

lnorms.hf<- function(n,v=5){
  X <- (multits.highfreq[,c(2,4)])[seq(n,v+n-1),]
  #print(X)
  Y <- ripsDiag(X, maxdimension = 1, maxscale = 5)
  land <- landscape(Diag=Y[["diagram"]],dimension=1,KK=1,tseq=tseq)
  t <- c((Norm(land,p=1)),(Norm(land,p=2)))
  return(t)
}
b=5
L.hf <- sapply(seq(1,length(multits.highfreq[,1])+1-b),lnorms.hf)
L.hf.time <- t(rbind(as.Date(index(multits.highfreq[-seq(1,13)])),L.hf))
L.hf.xts <- xts(t(L.hf), order.by = index(multits.highfreq[-seq(1,13)]),"%Y-%m-%d %H:%M:%S")

L.spin <- sapply(seq(1,2304),lnorms.hf)
L.hf.time <- t(rbind(as.Date(index(multits.highfreq[-seq(1,13)])),L.spin))
L.spin.xts <- xts(t(L.spin), order.by = index(multits.highfreq[-seq(1,13)]),"%Y-%m-%d %H:%M:%S")

par(mfrow=c(3,1))
chart_Series(spicejet.xts)
chart_Series(indigo.xts)
chart_Series(L.spin.xts)

par(mfrow=c(3,1))
chart_Series(spicejet.xts)
chart_Series(indigo.xts)
chart_Series(L.hf.xts)
