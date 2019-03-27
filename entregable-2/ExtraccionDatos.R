source("init.R")

santander <- stockGetdata("SAN.MC")
lastday <- tail(santander$Date, 1)

santander.df <- stockSubset(santander, "2011-01-01", lastday)
print(head(santander.df))

psan <- ggplot(santander.df, aes(x = Date, y = Adj.Close)) + geom_line()
print(psan)

ibex <- stockGetdata("^IBEX")
ibex.df <- stockSubset(ibex, "2011-01-01", lastday)

bbva <- stockGetdata("BBVA")
bbva.df <- stockSubset(bbva, "2011-01-01", lastday)

# mapmc <- stockGetdata("MAP.MC")
# mapmc.df <- stockSubset(mapmc, "2011-01-01", lastday)

compare.df <- data.frame(
  date = ibex.df$Date,
  ibex = relativePrice(ibex.df$Adj.Close),
  san = relativePrice(santander.df$Adj.Close),
  # map = relativePrice(mapmc.df$Adj.Close),
  bbva = relativePrice(bbva.df$Adj.Close)
)

print(head(compare.df))

comparePlot.df <- melt(compare.df, id = c("date"))
str(comparePlot.df)

pcomp <-
  ggplot(comparePlot.df, aes(x = date, y = value, colour = variable)) + geom_line() +
  scale_color_manual(values = c("yellow", "orange", "red", "brown"))

print(pcomp)
