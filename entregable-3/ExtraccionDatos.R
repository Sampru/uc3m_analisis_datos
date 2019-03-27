
source("init.R")

santander.df <- stockGetdata("SAN.MC")
lastday <- tail(santander.df$Date,1)

santander.df <- stockSubset(santander.df, "2011-01-01", lastday)
print(head(santander.df))

psan <- ggplot(santander.df, aes(x=Date, y=Adj.Close)) + geom_line()
print(psan)

ibex <- stockGetdata("^IBEX")
ibex.df <- stockSubset(ibex, "2011-01-01", lastday)

compare.df <- data.frame(date = ibex.df$Date,
                         ibex = relativePrice(ibex.df$Adj.Close),
                         san = relativePrice(santander.df$Adj.Close))

print(head(compare.df))

comparePlot.df <- melt(compare.df, id=c("date"))
str(comparePlot.df)

pcomp <- ggplot(comparePlot.df, aes(x=date, y=value, colour=variable)) + geom_line() + 
       scale_color_manual(values=c("black","red"))
   
print(pcomp)




