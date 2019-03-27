source("init.R")
library(gridExtra)

ibex = stockGetdata("^IBEX")
lastday = tail(ibex$Date, 1)
ibex.df = stockSubset(ibex, "2011-01-01", lastday)

santander.df = stockSubset(stockGetdata("SAN.MC"), "2011-01-01", lastday)
mapmc.df = stockSubset(stockGetdata("MAP.MC"), "2011-01-01", lastday)

ibxsan.df = data.frame(date = tail(ibex.df$Date, -1), ibex = stepReturn(ibex.df$Adj.Close), san = stepReturn(santander.df$Adj.Close))
ibxmap.df = data.frame(date = tail(ibex.df$Date, -1), ibex = stepReturn(ibex.df$Adj.Close), map = stepReturn(mapmc.df$Adj.Close))

ibxsan.cmp = melt(ibxsan.df, id = c("date"))
ibxmap.cmp = melt(ibxmap.df, id = c("date"))

comp1 =  ggplot(ibxsan.cmp, aes(x = date, y = value, colour = variable)) +
                geom_point() +
                scale_color_manual(values = c("blue", "green"))
comp2 =  ggplot(ibxmap.cmp, aes(x = date, y = value, colour = variable)) +
                geom_point() +
                scale_color_manual(values = c("purple", "red"))

grid.arrange(comp1, comp2, nrow = 1)
