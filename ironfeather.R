set.seed(303)

library(Quandl)

GBP_USD <- Quandl("GOOG/NYSE_GBB") #get the daily historical GBP/USD exchange rate
names(GBP_USD) <- c("date", "GBPUSD_Open", "GBPUSD_High", "GBPUSD_Low", "GBPUSD_Close", "GBPUSD_Volume")
JPY_USD <- Quandl("GOOG/NYSE_JYN")
names(JPY_USD) <- c("date", "JPYUSD_Open", "JPYUSD_High", "JPYUSD_Low", "JPYUSD_Close", "JPYUSD_Volume")
EUR_USD <- Quandl("GOOG/NYSE_ERO")
names(EUR_USD) <- c("date", "EURUSD_Open", "EURUSD_High", "EURUSD_Low", "EURUSD_Close", "EURUSD_Volume")

corpus <- merge(merge(GBP_USD, JPY_USD, by = "date"), EUR_USD, by = "date")
corpus <- na.omit(corpus)     # keep only rows with full data, discard a bit of data this way
tall <- dim(corpus)[1] # how any rows are there?

# we will use tall to determine how many colors each palette has

#define palletes
GU_color <- rainbow(tall, start=.5, end=.55)
JU_color <- rainbow(tall, start=.6, end=.63)
EU_color <- rainbow(tall, start=.7, end=.72)

plot.new()
par(bg = 'light blue')
# I throw -1 (sign) in at random to give more coverage to the piece

for(x_coord in 1:tall){
  sign <- sample(c(1,-1), 1) 
  abline(a = -1 * sign * (x_coord/tall), b = sign * corpus$GBPUSD_High[x_coord]/50, 
         lwd = .2*corpus$GBPUSD_Volume[x_coord]/mean(corpus$GBPUSD_Volume),
         col = GU_color[tall]) # GU
  sign <- sample(c(1,-1), 1) 
  abline(a = -1 * sign *(x_coord/tall), b = sign * corpus$JPYUSD_High[x_coord]/50, 
         lwd = .2*corpus$JPYUSD_Volume[x_coord]/mean(corpus$JPYUSD_Volume),
         col = JU_color[tall]) # JU
  sign <- sample(c(1,-1), 1) 
  abline(a = -1 * sign *(x_coord/tall), b = sign * corpus$EURUSD_High[x_coord]/50, 
         lwd = .2*corpus$EURUSD_Volume[x_coord]/mean(corpus$EURUSD_Volume),
         col = EU_color[tall]) # EU
  
}

