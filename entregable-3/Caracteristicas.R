source("init.R")

dax <- stockGetdata("^GDAXI")

dax.df <- stockSubset(dax, "2011-01-01", tail(dax$Date,1))

## Punto 1

punto1.df = data.frame(date   = dax.df$Date,
                       prices = dax.df$Adj.Close,
                       ema    = exponentialMovingAverage(dax.df$Adj.Close, 50),
                       sma    = SMA(dax.df$Adj.Close, 50))

punto1.melt = melt(punto1.df, id=c("date"))
punto1.plot = ggplot(punto1.melt, aes(x=date, y=value, colour=variable)) + geom_line() + 
  scale_color_manual(values=c("red","green", "blue"))

print(punto1.plot)

## Punto 2

punto2.df = data.frame(date = dax.df$Date,
  rentFut30 = rentabilidadFutura(dax.df$Adj.Close, 30),   
  rentPas30 = rentabilidadPasada(dax.df$Adj.Close, 30),
  rentFut5 = rentabilidadFutura(dax.df$Adj.Close, 5),   
  rentPas5 = rentabilidadPasada(dax.df$Adj.Close, 5),
  rentLog = rendimientoLogaritmico(dax.df$Adj.Close),
  smatrend = smaTrend(dax.df$Adj.Close, 50),
  ATR = ATR(dax.df$High, dax.df$Low, dax.df$Adj.Close))

## Punto 3

pairs(punto2.df, labels = colnames(punto2.df), main = "Pairs matrix", pch = 21,
  bg = c("red", "green", "blue", "yellow"), upper.panel = NULL)