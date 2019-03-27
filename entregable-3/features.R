# Analisis de Datos
# Master Finctech UC3m
#
# Author: Tomas de la Rosa


# Computes the return to 'gap' days ahead
fwdReturns <- function(priceVector, gap){
  l <- length(priceVector)
  returns <- priceVector[(gap+1):l]/priceVector[1:(l-gap)] - 1
  
  returns[(l-gap+1):l] <- rep(NA_real_, gap)
  return(returns)
}

# Computes the returns from 'gap' days to present
backReturns <- function(priceVector, gap){
  l <- length(priceVector)
  returns <- priceVector[(gap+1):l]/priceVector[1:(l-gap)] - 1
  
  serierets <- c(rep(NA_real_, gap), returns)
  return(serierets)
  
}


# Computes the Simple moving average
# <prices> is matrix with price colum vectors
# <n> the size of the sliding window
movingAverage <- function(prices, n){
  ticks <- length(prices)
  ma <- rep(NA_real_, times=ticks)
  
  for (i in n:ticks){
    slidewindow <- prices[(i - n + 1):i]
    ma[i] <- mean(slidewindow)
  }
  return(ma)
  
}

SMA <- function (prices, n){
  MA <- movingAverage(prices, n)
  return(MA)
}

SMA_dev <- function (prices, n){
  MA <- movingAverage(prices, n)
  ma_dev <- prices/MA - 1
  return(ma_dev)
}

# EMA computation
exponentialMovingAverage <- function(prices, n) {
  ticks = length(prices)
  ema = rep(NA_real_, times=ticks)
  k = (2/(n+1))
  ema[1] = movingAverage(prices, 1)[1]
  
  for (i in 2:ticks){
    ema[i] = ema[i-1] + (k * (prices[i] - ema[i-1])) 
  }
  
  return(ema)
}

# Stochastic oscillator
stochasticOscillator <- function(prices){
  ticks = length(prices)
  max = max(prices, na.rm = TRUE)
  min = min(prices, na.rm = TRUE)
  s = rep(NA_real_, times=ticks)
  
  for (i in 1:ticks) {
    s[i] = 100 * ((prices[i] - min)/(max - min))
  }
  
  return(s)
}

# Rentabilidad futura
rentabilidadFutura <- function(prices, n){
  rf = rep(NA_real_, length(prices))
  for (i in 1:length(prices)-n){
    rf[i] = (prices[i+n]/prices[i])-1
  }
  return(rf)
}

# Rentabilidad pasada
rentabilidadPasada <- function(prices, n){
  rp = rep(NA_real_, length(prices))
  for (i in (n+1):length(prices)){
    rp[i] = (prices[i]/prices[i-n])-1
  }
  return(rp)
}

# Rendimiento logaritmico
rendimientoLogaritmico <- function(prices){
  rl = rep(NA_real_, length(prices))
  for (i in 2:length(prices)){
    rl[i] = log(prices[i]/prices[i-1])
  }
  return(rl)
}

# SMA trend
smaTrend <- function(prices, n){
  sma = SMA(prices, n)
  st = rep(NA_real_, length(sma))
  for (i in 2:length(sma)){
    st[i] = (sma[i]/sma[i-1])-1
  }
  return(st)
}

# Rango verdadero
ATR <- function(maxs, mins, prices){
  atr = vector()
  for (i in 2:length(prices)){
    atr[i] = max(maxs[i]-mins[i], maxs[i]-prices[i-1], prices[i-1]-mins[i])
  }
  return(atr)
}