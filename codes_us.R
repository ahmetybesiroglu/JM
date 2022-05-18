
library(quantmod)
library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(plotly)
library(TSstudio)
library(ggthemes)
library(tidyr)
library(forecast)
library(tseries)
library(urca)
library(zoo)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(pastecs)
library(funModeling)


Symbols <- unique(sort(c("AAPL", "MSFT", "GOOG", "AMZN", 
                         "TSLA", "FB", "NFLX", "NVDA")))

#Symbols <- unique(sort(c("AAPL", "MSFT", "GOOG", "AMZN", 
#                         "TSLA", "JNJ", "FB", "NVDA",
#                         "V", "WMT", "PG", "JPM",
#                         "XOM", "MA", "CVX", "BAC",
#                         "KO", "PFE", "PEP", "ABT",
#                         "ACN", "NKE", "T", "NFLX")))


ENV.STOCK <- new.env()

getSymbols(Symbols, 
           from = "2022-01-01", to = Sys.Date(), 
           auto.assign = TRUE,
           env = ENV.STOCK)

getSymbols("NDAQ", 
           from = "2013-01-01", to = Sys.Date(), 
           auto.assign = TRUE)



XTS.ADJUSTED <- do.call(merge, eapply(ENV.STOCK, Ad))
XTS.ADJUSTED <- XTS.ADJUSTED[,sort(names(XTS.ADJUSTED))]

XTS.VOLUME <- do.call(merge, eapply(ENV.STOCK, Vo))
XTS.VOLUME <- XTS.VOLUME[,sort(names(XTS.VOLUME))]

ts_plot(XTS.ADJUSTED, slider = TRUE)

#daily_returns
XTS.DRETURNS <- lapply(XTS.ADJUSTED, dailyReturn)
XTS.DRETURNS <- do.call(merge, XTS.DRETURNS)
names(XTS.DRETURNS) <- Symbols
XTS.DRETURNS <- XTS.DRETURNS[-1,]

ts_plot(XTS.DRETURNS, slider = TRUE)

#weekly_returns
XTS.WRETURNS <- lapply(XTS.ADJUSTED, weeklyReturn)
XTS.WRETURNS <- do.call(merge, XTS.WRETURNS)
names(XTS.WRETURNS) <- Symbols
XTS.WRETURNS <- XTS.WRETURNS[-1,]

ts_plot(XTS.WRETURNS, slider = TRUE)

#monthly_returns
XTS.MRETURNS <- lapply(XTS.ADJUSTED, monthlyReturn)
XTS.MRETURNS <- do.call(merge, XTS.MRETURNS)
names(XTS.MRETURNS) <- Symbols
XTS.MRETURNS <- XTS.MRETURNS[-1,]

ts_plot(XTS.MRETURNS, slider = TRUE)


# Sums --------------------------------------------------------------------
weekly_closing_mean <- apply.weekly(XTS.ADJUSTED, function(x) apply(x, 2, mean))
monthly_closing_mean <- apply.monthly(XTS.ADJUSTED, function(x) apply(x, 2, mean))

weekly_volume_sums <- apply.weekly(XTS.VOLUME, function(x) apply(x, 2, sum))
monthly_volume_sums <- apply.monthly(XTS.VOLUME, function(x) apply(x, 2, sum))

# Summary -----------------------------------------------------------------

summary(XTS.ADJUSTED)
psych::describe(XTS.ADJUSTED)
pastecs::stat.desc(XTS.ADJUSTED)
psych::describe(XTS.DRETURNS)
pastecs::stat.desc(XTS.ADJUSTED)
funModeling::profiling_num(XTS.ADJUSTED)
funModeling::profiling_num(XTS.DRETURNS)


# Charting ----------------------------------------------------------------

chartSeries(ENV.STOCK$AAPL, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()

chartSeries(ENV.STOCK$AMZN, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()

chartSeries(ENV.STOCK$FB, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()

chartSeries(ENV.STOCK$GOOG, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()

chartSeries(ENV.STOCK$MSFT, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()

chartSeries(ENV.STOCK$NFLX, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()

chartSeries(ENV.STOCK$NVDA, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()


chartSeries(ENV.STOCK$TSLA, 
            type = "candlestick", 
            theme = chartTheme("white"))
addSMA(n = 15, col = "red")
addVolatility()
addZigZag()
addROC()
addRSI()



chart_Series(Cl(ENV.STOCK$FB))
add_TA(Cl(ENV.STOCK$GOOG), on = 1)
add_RSI()

normalise_series <- function(xdat) xdat / coredata(xdat)[1]
mytheme <- chart_theme()
chart_Series(normalise_series(Cl(ENV.STOCK$FB)) - 1, theme = mytheme)
add_TA(normalise_series(Cl(ENV.STOCK$GOOG)) - 1, on = 1, col = "red", lty = 1)



# Visualization -----------------------------------------------------------

#from wide to long
#level
DF.ADJUSTED <- as.data.frame(XTS.ADJUSTED)[sort(names(XTS.ADJUSTED))]
DF.ADJUSTED <- rownames_to_column(DF.ADJUSTED, var = "Date")
DF.ADJUSTED$Date <- as.Date(DF.ADJUSTED$Date)

LONG.DF.ADJUSTED <- gather(DF.ADJUSTED, Stock, Value, -Date, factor_key = FALSE)

#visualization
ggplot(data = LONG.DF.ADJUSTED, aes(x=as.Date(Date), y=Value,colour = factor(Stock), group = 1)) + 
  geom_line() + facet_wrap(~Stock, scales = "free") + 
  theme(legend.position = "none") 




#from wide to long

#daily returns
DF.DRETURNS <- as.data.frame(XTS.DRETURNS)[sort(names(XTS.DRETURNS))]
DF.DRETURNS <- rownames_to_column(DF.DRETURNS, var = "Date")
DF.DRETURNS$Date <- as.Date(DF.DRETURNS$Date)

LONG.DF.DRETURNS <- gather(DF.DRETURNS, Stock, Returns, -Date, factor_key = FALSE)

#summary
SUMMARY.DRETURNS <- LONG.DF.DRETURNS %>% group_by(Stock) %>% 
  summarise(Mean = mean(Returns),Median = median(Returns), StDev = sd(Returns))

#visualization
ggplot(data = LONG.DF.DRETURNS, aes(x=as.Date(Date), y=Returns,colour = factor(Stock), group = 1)) + 
  geom_line() + facet_wrap(~Stock, scales = "fixed") + 
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b %y")
#boxplot
LONG.DF.DRETURNS %>%  group_by(Stock) %>% mutate(Mean = mean(Returns), Median = median(Returns)) %>% 
  ggplot(aes(x=Returns, y=reorder(Stock, Median), fill = Stock)) +
  geom_boxplot() + theme(legend.position = "none")
#ridgeplot
LONG.DF.DRETURNS %>%  group_by(Stock) %>% mutate(Mean = mean(Returns)) %>% 
  ggplot(aes(x=Returns, y=reorder(Stock, Mean), fill = Stock)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  theme(legend.position = "none")


#weekly returns
DF.WRETURNS <- as.data.frame(XTS.WRETURNS)[sort(names(XTS.WRETURNS))]
DF.WRETURNS <- rownames_to_column(DF.WRETURNS, var = "Date")
DF.WRETURNS$Date <- as.Date(DF.WRETURNS$Date)

LONG.DF.WRETURNS <- gather(DF.WRETURNS, Stock, Returns, -Date, factor_key = FALSE)

#summary
SUMMARY.WRETURNS <- LONG.DF.WRETURNS %>% group_by(Stock) %>% 
  summarise(Mean = mean(Returns),Median = median(Returns), StDev = sd(Returns))

#visualization
ggplot(data = LONG.DF.WRETURNS, aes(x=as.Date(Date), y=Returns,colour = factor(Stock), group = 1)) + 
  geom_line() + facet_wrap(~Stock, scales = "fixed") + 
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b %y")
#boxplot
LONG.DF.WRETURNS %>%  group_by(Stock) %>% mutate(Mean = mean(Returns), Median = median(Returns)) %>% 
  ggplot(aes(x=Returns, y=reorder(Stock, Median), fill = Stock)) +
  geom_boxplot() + theme(legend.position = "none")

LONG.DF.WRETURNS %>%  group_by(Stock) %>% mutate(Mean = mean(Returns)) %>% 
  ggplot(aes(x=Returns, y=reorder(Stock, Mean), fill = Stock)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  theme(legend.position = "none")




#monthly returns
DF.MRETURNS <- as.data.frame(XTS.MRETURNS)[sort(names(XTS.MRETURNS))]
DF.MRETURNS <- rownames_to_column(DF.MRETURNS, var = "Date")
DF.MRETURNS$Date <- as.Date(DF.MRETURNS$Date)

LONG.DF.MRETURNS <- gather(DF.MRETURNS, Stock, Returns, -Date, factor_key = FALSE)

#summary
SUMMARY.MRETURNS <- LONG.DF.MRETURNS %>% group_by(Stock) %>% 
  summarise(Mean = mean(Returns),Median = median(Returns), StDev = sd(Returns))

#visualization
ggplot(data = LONG.DF.MRETURNS, aes(x=as.Date(Date), y=Returns,colour = factor(Stock), group = 1)) + 
  geom_line() + facet_wrap(~Stock, scales = "fixed") + 
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b %y")
#boxplot
LONG.DF.MRETURNS %>%  group_by(Stock) %>% mutate(Mean = mean(Returns), Median = median(Returns)) %>% 
  ggplot(aes(x=Returns, y=reorder(Stock, Median), fill = Stock)) +
  geom_boxplot() + theme(legend.position = "none")

LONG.DF.MRETURNS %>%  group_by(Stock) %>% mutate(Mean = mean(Returns)) %>% 
  ggplot(aes(x=Returns, y=reorder(Stock, Mean), fill = Stock)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  theme(legend.position = "none")


adf.test(XTS.DRETURNS$FB)
fit1<- auto.arima(XTS.ADJUSTED$FB.Adjusted)
forecast(fit1) %>% plot


fit1<- auto.arima(XTS.DRETURNS$GOOG)
forecast(fit1) %>% plot


my_ts = ts(cds,frequency = 260.25, start = c(2007,3,27))
ms_de <- decompose(my_ts)
plot(ms_de)



par(mfrow = c(4, 2))
lapply(XTS.ADJUSTED, function(x) {
  fit <- auto.arima(x)
  fcast <- forecast(fit, h = 7)
  plot(fcast)
})


