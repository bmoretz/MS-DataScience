library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(GGally)
library(ggthemes)
library(formattable)
library(scales)
library(reshape2)
library(skimr)
library(gridExtra)
library(lessR)
library(ggiraphExtra)
library(olsrr)
library(caret)
library(sjPlot)
library(sjmisc)
library(car)
library(WVPlots)
library(MASS)
library(Metrics)
library(stringr)
library(Rtsne)
library(plotly)
library(Quandl)
library(tidyverse)
library(quantmod)
library(ggcorrplot)
library(tidyquant)
library(timetk)
library(tidyverse)
library(tibbletime)
library(forecast)
library(rugarch)
library(xts)

#####################################################################
######################### EDA #######################################
#####################################################################

data.path <- "D:/Projects/MSDS-Capstone/data"

setwd(data.path)

theme_set(theme_light())

# Theme Overrides
theme_update(axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "darkgreen"),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             plot.subtitle = element_text(face = "bold", size = 8, colour = "darkred"),
             legend.title = element_text(size = 12, color = "darkred", face = "bold"),
             legend.position = "right", legend.title.align=0.5,
             panel.border = element_rect(linetype = "solid", 
                                         colour = "lightgray"), 
             plot.margin = unit(c( 0.1, 0.1, 0.1, 0.1), "inches"))

# Data files
data.energy <- read_csv( file = "northwestern_energy_all_energy_ohlc.csv") 
data.symbology <- read_csv( file = "symbol_description.csv")

# Global Symbol look-up
trading.symbols <- colnames(data.energy)[
  sapply(colnames(data.energy), FUN = function(symbol) {
    !str_contains(symbol, "_")
  })]

data <- data.energy

# Utility Functions

getDataForSymbol <- function( symbol, data = data.energy ) {
  
  base.cols <- c("open", "high", "low", "close", "volume")
  
  symbol.cols <- c("nymex_date", as.vector(sapply(base.cols, function( c ) { 
    paste0(symbol, "_", c) }, simplify = T)))
  
  data.symbol <- data[, symbol.cols]
  data.symbol <- data.symbol[order(data.symbol$nymex_date),]
  
  colnames(data.symbol) <- as.vector(sapply(colnames(data.symbol), FUN = function(c) { 
    str_replace(c, paste0(symbol, "_"), "") }, simplify = T))
  
  data.symbol <- data.symbol[complete.cases(data.symbol),]
  data.symbol$spotPrice <- data.symbol[["close"]]
  
  # calculate returns from prices
  prices <- data.symbol$spotPrice
  n <- length(prices)
  ret <- prices[-1] / prices[-n] - 1
  
  # store it
  data.symbol$return <- c(0, ret)
  data.symbol$logReturn <- log(1 + data.symbol$return)
  
  colnames(data.symbol)
  
  data.symbol[-c(1:2),] # throw away the first record that has no return data.
}

plotReturns <- function( symbol, energy_data, start_date = "2019-1-1" ){ 
  
  data <- energy_data[[symbol]]
  data.plot <- data[data$nymex_date >= start_date, c("nymex_date", "spotPrice", "return", "logReturn")]
  
  p1 <- ggplot(data.plot, aes(nymex_date, return)) +
    geom_line()
  
  p2 <- ggplot(data.plot, aes(nymex_date, logReturn)) +
    geom_line()
  
  p3 <- ggplot(data.plot, aes(return, fill = ..count..)) +
    geom_histogram()
  
  p4 <- ggplot(data.plot, aes(logReturn, fill = ..count..)) +
    geom_histogram()
  
  grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("Returns for", symbol, " since ", start_date))
}

getRetVsT <- function(returns, df_canidates = c(1, 2, 4, 6, 10, 20)) {
  plots <- lapply(df_canidates, function(df) {
    
    n <- length(returns)
    q_range <- (1:n) / (n+1)
    
    data <- data.table(ret = returns, theoretical = qt(q_range, df))
    data$theoretical <- sort(data$theoretical)
    
    model <- lm(qt(c(0.25,0.75), df = df) ~ quantile(data$ret,c(0.25,0.75)))
    
    ggplot(data, aes(x = sort(ret), y = theoretical)) +
      geom_abline(col = 'cornflowerblue', lwd = 1.3, slope = model$coefficients[2], intercept = model$coefficients[1]) +
      geom_point() +
      labs(title = paste("df = ", df), 
           x = "returns",
           y = "theoretical")
  })
  
  do.call(grid.arrange, c(plots, top = "QQ-Plot: returns vs t-distribution"))
}

getRetDensityVsNorm <- function(returns) {
  data <- data.table(x = seq(min(returns), max(returns), length.out = length(returns)), y = returns)
  
  ggplot(data, aes(y)) +
    geom_density(aes(col = "KDE"), lwd = 1) +
    geom_line(aes(x, dnorm(x, mean = mean(returns), sd = sd(returns)), col = "normal(mean, sd)"), lwd = 1.1, linetype = "longdash") +
    geom_line(aes(x, dnorm(x, mean = median(returns), sd = mad(returns)), col = "normal(median, mad)"), lwd = 1.1, linetype = "dashed") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste("KDE: Returns Vs Normal, n=", nrow(data)), y = "density", x = "") +
    theme(legend.position = "right")
}

getRetNormQuantiles <- function(returns, quantiles = c(0.25,0.1,0.05,0.025,0.01,0.0025), desc = "") {
  plots <- lapply(quantiles, function(p) {
    
    p_value <- p
    n <- length(returns)
    q_range <- (1:n) / (n+1)
    
    data <- data.table(ret = returns, theoretical = qnorm(q_range))
    data$theoretical <- sort(data$theoretical)
    
    model <- lm(qnorm(c(p_value, 1-p_value)) ~ quantile(data$ret, c(p_value, 1-p_value)))
    
    ggplot(data, aes(x = sort(ret), y = theoretical)) +
      geom_abline(col = 'cornflowerblue', lwd = 1.3, slope = model$coefficients[2], intercept = model$coefficients[1]) +
      geom_point() +
      labs(title = paste("p = ", p_value), 
           x = "returns",
           y = "theoretical")
  })
  
  do.call(grid.arrange, c(plots, top = paste(desc, "vs Normal Quantiles")))
}

getCorr <- function( data = commodites ) {
  ret <- data.table()
  
  for(symbol in names(data)) {
    s <- data.table(return = data[[symbol]]$logReturn)
    colnames(s) <- symbol
    
    ret <- cbind(ret, s)
  }
  
  ggcorrplot(cor(ret),
             type = "lower",
             method = "circle",
             colors = c("tomato2", "white", "springgreen3"),
             lab_size = 3,
             title = "Energy Commodity Correlations")
}

candlestick <- function(symbol, start_date = "2019-1-1", data = commodites ) {
  
  desc <- data.symbology[data.symbology$Symbol == toupper(symbol),]$Description
  
  d <- data[[symbol]][data[[symbol]]$nymex_date >= start_date,]
  p <- d %>% 
    plot_ly(x = ~nymex_date, type = "candlestick",
            open = ~open, close = ~spotPrice,
            high = ~high, low = ~low) %>%
    add_lines(x = ~nymex_date, y = ~open, line = list(color = 'black', width = 0.75), inherit = F) %>%
    layout(title = paste(symbol, "activity since", start_date))
  
  v <- d %>% 
    plot_ly(x = ~nymex_date, y = ~volume, type='bar', name = "Volume",
            colors = c('#17BECF','#7F7F7F'))
  
  rs <- list(visible = TRUE, x = 0.5, y = -0.055,
             xanchor = 'center', yref = 'paper',
             font = list(size = 9),
             buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
             ))
  
  pp <- subplot(p, v, heights = c(0.7,0.2), nrows=2,
                shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste( desc, " : ", format(as.Date(start_date), "%b %d %Y"), "-", format(as.Date(max(d$nymex_date)), "%b %d %Y")),
           xaxis = list(rangeselector = rs),
           legend = list(orientation = 'h', x = 0.5, y = 1,
                         xanchor = 'center', yref = 'paper',
                         font = list(size = 10),
                         bgcolor = 'transparent'))
  pp
}

pretty_kable <- function(data, title, dig = 2) {
  kable(data, caption = title, digits = dig) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

getCorTable <- function( values ) {
  p.table <- data.table(round(values, 4), keep.rownames = T)
  
  formattable(p.table, align = c("l", "c", "c", "c", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
              ))
}

getHoldingsFromPositions <- function( positions ) {
  holdings <- list()
  for( index in 1:nrow(positions) ) {
    
    pos <- positions[index,]
    
    holding <- data.table( Date = dates, Price = 0 )
    
    open.dates <- holding[Date >= pos$EnterDate][Date <= pos$ExitDate]$Date
    
    holding[Date %in% open.dates]$Price <- pos$EnterPrice
    #holding[Date %in% pos$ExitDate]$Price <- pos$ExitPrice
    holding$Direction <- pos$Direction
    holding$PnL <- pos$ProfitLoss
    holding[Price == 0]$Price <- NA
    holdings[[index]] <- holding
  }
  
  holdings
}

candlestickHoldings <- function( symbol, holdings, data, return ) {
  
  desc <- data.symbology[data.symbology$Symbol == toupper(symbol),]$Description
  
  print(desc)
  
  start_date <- as.Date(min(data$Date))
  end_date <- as.Date(max(data$Date))
  
  p <- data %>% 
    plot_ly(x = ~Date, type = "candlestick",
            open = ~open, close = ~spotPrice,
            high = ~high, low = ~low)
  
  for(i in 1:length(holdings)){
    h <- holdings[[i]]
    
    p <- add_lines(p, x = h$Date, y = h$Price,
                   linetype = h$Direction,
                   line = list(color = ifelse(h[1]$PnL >= 0, "darkgreen", "darkred"), width = 2), 
                   text = paste("Profit / Loss: ", round(h[1]$PnL, 2)), 
                   inheret = F)
  }
  
  p
  
  rs <- list(visible = TRUE, x = 0.5, y = -0.055,
             xanchor = 'center', yref = 'paper',
             font = list(size = 9),
             buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
             ))
  
  pp <- subplot(p, heights = c(1), nrows=1,
                shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste( desc, " - Mean Revision Strategy Trading : ", format(start_date, "%b %d %Y"), "-", format(end_date, "%b %d %Y"), " Net Return : ", round(return, 2), "%"),
           xaxis = list(rangeselector = rs),
           legend = list(orientation = 'h', x = 0.5, y = 1,
                         xanchor = 'center', yref = 'paper',
                         font = list(size = 10),
                         bgcolor = 'transparent'))
  pp
}

###############################################################
# Indexes
##############################################################

indices <- colnames(data.energy)[colnames(data.energy) %in% data.symbology$Symbol]


###############################################################
# EDA
##############################################################

energy.commodities <- c("cl", "sc", "hp", "mt", "ng", "qa", "qg", "ve", "rb", "ho")

commodites <- lapply(energy.commodities, FUN = function( s ) {
  getDataForSymbol(s)
})

names(commodites) <- energy.commodities

getRetNormQuantiles(rnorm(2000), desc = "RNORM (baseline)") # for reference

## Symbol Simple Correlations (Pearson)

getCorr(commodites)

#### RB

plotReturns("rb", commodites)

rb.ret <- commodites$rb$logReturn

getRetVsT(rb.ret) # t-distribution, df = 6

getRetDensityVsNorm(rb.ret) # strongest fit: median / mad

getRetNormQuantiles(rb.ret, desc = "RO")

symbol <- "rb"
start_date <- "2019-6-1"

candlestick(symbol, start_date, commodites)

#### HO

plotReturns("ho", commodites)

ho.ret <- commodites$ho$logReturn

getRetVsT(ho.ret) # t-distribution, df = 4, watch out for the heavy tail

getRetDensityVsNorm(ho.ret) # strogest fit: median / mad

getRetNormQuantiles(ho.ret, desc = "HO")

symbol <- "ho"
start_date <- "2019-1-1"

candlestick(symbol, start_date, commodites)

#### CL

plotReturns("cl", commodites)

cl.ret <- commodites$cl$logReturn

getRetVsT(cl.ret) # t-distribution, df = 4, watch out for the heavy tail

getRetDensityVsNorm(cl.ret) # strogest fit: median / mad

getRetNormQuantiles(cl.ret, desc = "CL")

symbol <- "cl"
start_date <- "2019-6-1"

candlestick(symbol, start_date, commodites)

########
###### Crude

getReturnsForSymbol <- function( symbol, data = commodites ) {
  r <- as.data.table(commodites[[symbol]][, c("nymex_date", "logReturn")])[, .(Date = nymex_date, Return = logReturn)]
  colnames(r)[2] <- symbol
  
  r
}

brent <- getReturnsForSymbol("sc")
wti <- getReturnsForSymbol("cl")
gulf <- getReturnsForSymbol("mt")
ve <- getReturnsForSymbol("ve")
gas <- getReturnsForSymbol("rb")
natgas <- getReturnsForSymbol("ng")
ho <- getReturnsForSymbol("ho")

crude.wide <- merge(merge(merge(brent, wti, on = c("Date")), gulf, on = c("Date")), ve, on = c("Date"))

commodity.wide <- merge(merge(merge(crude.wide, gas, on = c("Date")), natgas, on = c("Date")), ho, on = c("Date"))

### Correlations

p <- cor(crude.wide[, 2:5], method = c("pearson"))
getCorTable(p)

k <- cor(crude.wide[, 2:5], method = c("kendall"))
getCorTable(k)

s <- cor(crude.wide[, 2:5], method = c("spearman"))
getCorTable(s)

crude.dt <- crude.wide[crude.wide$Date >= "2018-1-1" & crude.wide$Date <= "2018-12-31",]

ggplot(crude.dt, aes(x = Date)) +
  geom_line(aes(y = cl), lwd = .8, col = "cornflowerblue") +
  geom_line(aes(y = mt), lwd = .8, col = "orange", alpha = .7) +
  labs(title = "WTI vs Gulf Sour", y = "Return")

crude.dt <- crude.long[crude.wide$Date >= "2018-6-1" & crude.wide$Date <= "2018-8-31",]

ggplot(crude.dt, aes(x = Date)) +
  geom_line(aes(y = cl), lwd = .8, col = "cornflowerblue") +
  geom_line(aes(y = mt), lwd = .8, col = "orange", alpha = .7) +
  labs(title = "WTI vs Gulf Sour", y = "Return")


ggplot(crude.dt, aes(x = cl, y = mt)) +
  geom_point(col = "black") +
  geom_smooth() +
  labs(title = "WTI vs Gulf Sour", x = "WTI", y = "Gulf Sour")

##### Wide Format
## Dist of Returns

commodity.long <- melt(commodity.wide, id.var = c("Date"),
                       variable.name = "Symbol",
                       value.name = "Return")
commodity.long$Symbol <- toupper(commodity.long$Symbol)

data.symbology <- data.symbology[, 1:2]
commodity.long <- merge(commodity.long, data.symbology, on = c("Symbol"))

commodity.long <- commodity.long[, .(Date, Return, Symbol, Description)]

commodity.long %>%
  ggplot(aes(x = Return, fill = Description)) +
  #geom_histogram(aes(y = ..density..),alpha = 0.45, binwidth = 0.005) +
  geom_density(aes(y = ..density..), alpha = 0.35) +
  ggtitle("Monthly Returns Since 2014") +
  theme_update(plot.title = element_text(hjust = 0.5))

commodity.long %>%
  ggplot(aes(x = Return, fill = Description)) +
  geom_histogram(aes(y = ..density..),alpha = 0.45, binwidth = 0.005) +
  ggtitle("Monthly Returns Since 2014") +
  theme_update(plot.title = element_text(hjust = 0.5))


getRetVsT(commodites$ve$logReturn, 1:6)

commodity.long %>%
  ggplot(aes(x = Return, y = ..density..)) +
  geom_density( alpha = 1) +
  geom_histogram(aes(fill = Description), alpha = 0.45, binwidth = 0.01) +
  facet_wrap(~Symbol) +
  ggtitle("Monthly Returns Density Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

unique(commodity.long$Symbol)

commodity.long[Symbol != 'HO'] %>%
  ggplot(aes(x = Return, y = ..density..)) +
  geom_density( alpha = 1) +
  geom_histogram(aes(fill = Description), alpha = 0.45, binwidth = 0.01) +
  facet_wrap(~Symbol) +
  ggtitle("Returns Density Since 2013") +
  xlab("Returns") +
  ylab("Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

commodity.long[, Growth := cumsum(Return), by = Symbol]
commodity.long[, Cum := (1 + exp(Growth))]

commodity.long[Symbol != "VE"] %>%
  ggplot(aes(x = Date, y = Growth)) +
  geom_line(aes(col = Description)) +
  ggtitle("Growth") +
  xlab("Date") +
  theme_update(plot.title = element_text(hjust = 0.5))

commodity.long[Symbol != "VE"] %>%
  ggplot(aes(x = Date, y = Cum)) +
  geom_line(aes(col = Description)) +
  ggtitle("Growth") +
  xlab("Date") +
  theme_update(plot.title = element_text(hjust = 0.5))


# rolling sd

window <- 10

# rolling sd, tidyverse
sd_roll_10 <- rollify(sd, window = window)

commodity.rolling.sd <- 
  commodity.long %>%
  as_tbl_time(index = Date) %>%
  mutate(rolling_sd = sd_roll_10(Return)) %>%
  select(-Return) %>%
  na.omit()

commodity.rolling.sd[commodity.rolling.sd$Symbol != "VE",] %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = rolling_sd, color = Description)) +
  facet_wrap(aes(Description)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Volatility - 10 day", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

#####################
#### TS ACF
####################

ggAcf(ts(commodites$sc$logReturn, frequency = 365)) +
  ggtitle("Series: WTI")

ggAcf(ts(commodites$ng$logReturn, frequency = 365)) +
  ggtitle("Series: Natural Gas")

ggAcf(ts(commodites$ng$logReturn, frequency = 365)) +
  ggtitle("Series: Natural Gas")

############
### TS Correlations
###########

ggAcf(brent) + 
  ggtitle("Series: Brent Crude")

ggAcf(wti) +
  ggtitle("Series: WTI")

ggAcf(commodites$sc$logReturn) +
  ggtitle("Series: WTI")

ggAcf(commodity.wide[, .(sc, cl)]) +
  ggtitle("WTI vs Brent ACF")

ggAcf(commodity.wide[, .(cl, mt)]) +
  ggtitle("WTI vs Gulf Sour ACF")

############
### TS Models
###########

# WTI

sc.baseline <- as.data.table(commodites[["sc"]])
sc.baseline[, 
            Date := nymex_date][, 
                                nymex_date := NULL]

Box.test(sc.baseline$return, lag = 4, type = "Ljung-Box")

ggAcf(sc.train.data$return) +
  ggtitle("WTI Autocorrelation")

ggAcf(sc.train.data$return)$data

sc.train.data <- sc.baseline[Date < "2019-1-1"]
sc.test.data <- sc.baseline[Date >= "2019-1-1"]

sc.train.model <- auto.arima(sc.train.data$return, ic = "bic")

sc.test.model <- Arima(sc.test.data$return, model = sc.train.model)

sc.test.data$pred <- fitted(sc.test.model)

ggplot(sc.test.data, aes(x = Date)) +
  geom_line(aes(y = return), lwd = .5, col = "black") +
  geom_line(aes(y = pred), lwd = 1.5, col = "cornflowerblue", alpha = .7, linetype = 2) +
  geom_hline(aes(yintercept = .015), col = "green", lwd = .8, alpha = .7) +
  geom_hline(aes(yintercept = -.02), col = "red", lwd = .8, alpha = .7) +
  labs(title = "WTI Actual vs. Pred", y = "Return")

monthly <- sc.test.data[Date >= "2019-1-1" & Date < "2019-2-1"]

ggplot(monthly, aes(x = Date)) +
  geom_line(aes(y = return), lwd = .5, col = "black") +
  geom_line(aes(y = pred), lwd = 1.5, col = "cornflowerblue", alpha = .7, linetype = 2) +
  labs(title = "WTI Actual vs. Pred", y = "Return")

threshold <- 0.01

sc.test.data[, index := .I]

sc.enter <- sc.test.data[pred < -threshold | pred > threshold]
sc.enter$exit <- sc.enter$index + 4
sc.enter$side <- ifelse(as.numeric(sc.enter$pred) >= 0, "sell", "buy")

sc.exit <- sc.test.data[index %in% sc.enter$exit]
sc.exit$exit <- 0
sc.exit$side <- ifelse(sc.enter$side == "buy", "sell", "buy")

# Convert Transactions to Positions

sc.transactions <- merge(sc.enter, sc.exit, by.x = c("exit"), by.y = c("index"))
sc.positions <- sc.transactions[, .(Position = .I,
                                    Direction = ifelse(side.x == "buy", "Long", "Short"),
                                    EnterDate = Date.x,
                                    EnterPrice = spotPrice.x, 
                                    ExitDate = Date.y,
                                    ExitPrice = spotPrice.y)]
sc.positions[, ProfitLoss := ifelse(Direction == "Long", ExitPrice - EnterPrice, EnterPrice - ExitPrice)]
sc.positions[, Return := ifelse(Direction == "Long", (ExitPrice - EnterPrice)/ExitPrice, (EnterPrice - ExitPrice)/EnterPrice)]

sum(sc.positions$ProfitLoss)

sc.return <- merge(sc.test.data, sc.positions, by.x = c("Date"), by.y = c("EnterDate"), all.x = T)
sc.return[is.na(sc.return$Return)] <- 0

sc.return <- (cumprod(1 + sc.return$Return) - 1)[nrow(sc.test.data)] * 100
sc.return <- ( sc.return ^ 1/12) * 12

sc.ret <- (cumprod(1 + sc.test.data$return) - 1)[nrow(sc.test.data)] * 100

write.csv(sc.test.data, file = "sc.return.csv")

write.csv(sc.transactions, file = "sc.return.csv")

sc.transactions <- rbind(sc.enter, sc.exit)[ order(Date)][, .(Date, spotPrice, side)]

sc.transactions

write.csv(sc.transactions, file = "sc.trades.csv")

sc.disp.trans <- merge(sc.test.data, sc.transactions, by = c("Date"), all.x = T)
sc.disp.trans$color <- ifelse(sc.disp.trans$side == "buy", "green", ifelse(sc.disp.trans$side == "sell", "red", NA))

sc.disp.trans$spotPrice.y[is.na(sc.disp.trans$spotPrice.y)] <- NA

head(sc.disp.trans, 10)

sc.test.data[, return := (open - close)/close]

ggplot(sc.test.data[Date < "2019-2-1"]) +
  geom_line(aes(x = Date, y = close), lwd = 1, col = "black") +
  geom_point(data = sc.disp.trans[Date < "2019-2-1"], aes(x = Date, y = spotPrice.y, col = side), lwd = 5) +
  labs(title = "WTI Actual vs. Pred", y = "Return") +
  theme(legend.position = "none")

symbol <- "sc"
start_date <- "2019-1-1"

candlestick(symbol, start_date, commodites)

dates <- sc.test.data$Date
sc.holdings <- getHoldingsFromPositions(sc.positions)

write.csv(sc.holdings, file = "sc.positions.csv")

candlestickHoldings("cl", sc.holdings, sc.test.data, 18.79 )

# Brent Crude

cl.baseline <- as.data.table(commodites[["cl"]])
cl.baseline[, 
            Date := nymex_date][, 
                                nymex_date := NULL]

symbol <- "sc"
start_date <- "2014-1-1"

candlestick(symbol, start_date, commodites)

Box.test(cl.baseline$return, lag = 15, type = "Ljung-Box")

ggAcf(cl.train.data$return) +
  ggtitle("Brent Autocorrelation")

ggAcf(cl.train.data$return)$data

cl.train.data <- cl.baseline[Date < "2019-1-1"]
cl.test.data <- cl.baseline[Date >= "2019-1-1"]

plot.ts(cl.train.data)

ggplot(crude.wide, aes(x = Date)) +
  geom_line(aes(y = cl), lwd = .8, col = "cornflowerblue") +
  labs(title = "Brent", y = "Return")

ggAcf(cl.train.data$Return)
ggAcf(cl.train.data$Return)$data

cl.train.model <- auto.arima(cl.train.data$return, max.p = 10, max.q = 10, d = 1, ic = "bic")

cl.test.model <- Arima(cl.test.data$return, model = cl.train.model)

cl.test.data$pred <- fitted(cl.test.model)

cl.threshold <- 0.0175

ggplot(cl.test.data, aes(x = Date)) +
  geom_line(aes(y = return), lwd = .7, alpha = .6, col = "black") +
  geom_line(aes(y = pred), lwd = 1.5, col = "cornflowerblue", alpha = .7, linetype = 3) +
  geom_hline(aes(yintercept = cl.threshold), col = "darkred", lwd = .8, alpha = .7) +
  geom_hline(aes(yintercept = -cl.threshold), col = "darkgreen", lwd = .8, alpha = .7) +
  labs(title = "Actual vs. Pred", y = "Return") +
  scale_y_continuous(limits = c(-.05, .05))

monthly <- cl.test.data[Date >= "2019-1-1" & Date < "2019-2-1"]

ggplot(monthly, aes(x = Date)) +
  geom_line(aes(y = return), lwd = .5, col = "black") +
  geom_line(aes(y = pred), lwd = 1.5, col = "cornflowerblue", alpha = .7, linetype = 2) +
  labs(title = "Actual vs. Pred", y = "Return")

cl.test.data[, index := .I]

cl.enter <- cl.test.data[pred < -threshold | pred > threshold]
cl.enter <- cl.enter[Date < "2019-09-27"]
cl.enter$exit <- cl.enter$index + 1
cl.enter$side <- ifelse(as.numeric(cl.enter$pred) >= 0, "sell", "buy")

cl.exit <- cl.test.data[index %in% cl.enter$exit]
cl.exit$exit <- 0
cl.exit$side <- ifelse(cl.enter$side == "buy", "sell", "buy")

# Convert Transactions to Positions

cl.transactions <- merge(cl.enter, cl.exit, by.x = c("exit"), by.y = c("index"))
cl.positions <- cl.transactions[, .(Position = .I,
                                    Direction = ifelse(side.x == "buy", "Long", "Short"),
                                    EnterDate = Date.x,
                                    EnterPrice = spotPrice.x, 
                                    ExitDate = Date.y,
                                    ExitPrice = spotPrice.y)]
cl.positions[, ProfitLoss := ifelse(Direction == "Long", ExitPrice - EnterPrice, EnterPrice - ExitPrice)]
cl.positions[, Return := ifelse(Direction == "Long", (ExitPrice - EnterPrice)/ExitPrice, (EnterPrice - ExitPrice)/EnterPrice)]

sum(cl.positions$ProfitLoss)

cl.return <- merge(cl.test.data, cl.positions, by.x = c("Date"), by.y = c("EnterDate"), all.x = T)
cl.return[is.na(cl.return$Return)]$Return <- 0
cl.return <- (cumprod(1 + cl.return$Return) - 1)[nrow(cl.test.data)] * 100

cl.return <- ( cl.return ^ 1/9 ) * 12

write.csv(cl.transactions, file = "cl.trades.csv")

cl.disp.trans <- merge(cl.test.data, cl.transactions, by.x = c("Date"), by.y = c("EnterDate"), all.x = T)
cl.disp.trans$color <- ifelse(cl.disp.trans$side == "buy", "green", ifelse(cl.disp.trans$side == "sell", "red", NA))

cl.disp.trans$spotPrice.y[is.na(cl.disp.trans$spotPrice.y)] <- NA

head(cl.disp.trans, 10)

ggplot(cl.test.data[Date < "2019-2-1"]) +
  geom_line(aes(x = Date, y = close), lwd = 1, col = "black") +
  geom_point(data = cl.disp.trans[Date < "2019-2-1"], aes(x = Date, y = spotPrice.y, col = side), lwd = 5) +
  labs(title = "WTI Actual vs. Pred", y = "Return") +
  theme(legend.position = "none")


getHoldingsFromPositions <- function( positions ) {
  holdings <- list()
  for( index in 1:nrow(positions) ) {
    
    pos <- positions[index,]
    
    holding <- data.table( Date = dates, Price = 0 )
    
    open.dates <- pos[Date >= pos$EnterDate][Date <= pos$ExitDate]$Date
    
    print(open.dates)
    
    holding[Date %in% open.dates]$Price <- pos$EnterPrice
    holding[Date %in% pos$ExitDate]$Price <- pos$ExitPrice
    holding$Direction <- pos$Direction
    holding$PnL <- pos$ProfitLoss
    holding[Price == 0]$Price <- NA
    
    holdings[[index]] <- holding
  }
  
  holdings
}

write.csv(cl.positions, file = "cl.positions.csv")

cl.holdings <- getHoldingsFromPositions(cl.positions)


write.csv(cl.holdings, file = "cl.positions.csv")

candlestickHoldings("sc", cl.holdings, cl.test.data, cl.return )


# CL GARCH

par(mfrow=c(1,1))
plot(forecast(sc.test.model, h = 10))

arma.garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                             variance.model=list(garchOrder=c(1,1)))

cl.xts <- as.xts.data.table(cl.train.data[, .(Date, return)])
sc.xts <- as.xts.data.table(sc.train.data[, .(Date, return)])

sc.garch.norm = ugarchfit(data=cl.xts, spec=arma.garch.norm)

show(cl.garch.norm)

e = residuals(cl.garch.norm, standardize=TRUE)

fitdistr(e,"t")

arma.garch.t = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                             variance.model=list(garchOrder=c(1,1)),
                             distribution.model = "std")

cl.garch.t = ugarchfit( data = cl.xts, spec = arma.garch.t)
sc.garch.t <- ugarchfit( data = sc.xts, spec = arma.garch.t)

show(sc.garch.t)
plot(sc.garch.t)

show(cl.garch.t)

plot(cl.garch.t)

par(mfrow = c(3,2))
for(i in c(1, 2, 5, 6, 7, 13)) plot(cl.garch.t, which=i)

par(mfrow=c(1,1))
plot(cl.garch.t, which=4)

par(mfrow=c(2,1))
plot(cl.garch.t, which=3)
plot(cl.garch.t, which=2)

par(mfrow=c(2,1))
plot(cl.garch.t, which=11)
plot(cl.garch.t, which=8)

par(mfrow = c(3,2))
for(i in c(1,3,10,11,8,9)) plot(cl.garch.t, which=i)

cl.garch.norm = ugarchfit(data=cl.train.data, spec=garch.norm)
par(mfrow=c(1,1))
pred1 = ugarchforecast(cl.garch.norm, data = cl.train.data, n.ahead = 50)
plot(pred1, which = 1)

head(fitted(pred1))
head(sigma(pred1))


holdings <- list()

for( index in 1:nrow(positions()) ) {
  
  pos <- positions[index,]
  
  holding <- data.table( Date = dates, Price = 0 )
  
  
  holdings[[index]] <- holding
}


open.dates <- pos[Date >= pos$EnterDate][Date <= pos$ExitDate]$Date

print(open.dates)

holding[Date %in% open.dates]$Price <- pos$EnterPrice
#holding[Date %in% pos$ExitDate]$Price <- pos$ExitPrice
holding$Direction <- pos$Direction
holding$PnL <- pos$ProfitLoss
holding[Price == 0]$Price <- NA



# Gasoline


rb.baseline <- as.data.table(commodites[["rb"]])
rb.baseline[, 
            Date := nymex_date][, 
                                nymex_date := NULL]

rb.train.data <- rb.baseline[Date < "2019-1-1"]
rb.test.data <- rb.baseline[Date >= "2019-1-1"]

ggAcf(rb.train.data$return) +
  ggtitle("WTI Autocorrelation")

ggAcf(rb.train.data$return)$data

Box.test(rb.baseline$return, lag = 4, type = "Ljung-Box")

rb.train.model <- auto.arima(rb.train.data$return, ic = "bic",
                             max.p = 4,
                             max.q = 1,
                             d = 1.5)

rb.test.model <- Arima(sc.test.data$return, model = rb.train.model)
rb.test.data$pred <- fitted(rb.test.model)

ggplot(rb.test.data, aes(x = Date)) +
  geom_line(aes(y = return), lwd = .5, col = "black") +
  geom_line(aes(y = pred), lwd = 1.5, col = "cornflowerblue", alpha = .7, linetype = 2) +
  geom_hline(aes(yintercept = .015), col = "green", lwd = .8, alpha = .7) +
  geom_hline(aes(yintercept = -.02), col = "red", lwd = .8, alpha = .7) +
  labs(title = "WTI Actual vs. Pred", y = "Return")

threshold <- 0.015

rb.test.data[, index := .I]

rb.enter <- rb.test.data[pred < -threshold | pred > threshold]
rb.enter$exit <- rb.enter$index + 3

rb.enter <- rb.enter[exit < max(rb.test.data$index)]

rb.enter$side <- ifelse(as.numeric(rb.enter$pred) >= 0, "buy", "sell")

rb.exit <- rb.test.data[index %in% rb.enter$exit]
rb.exit$exit <- 0
rb.exit$side <- ifelse(rb.enter$side == "buy", "sell", "buy")

# Convert Transactions to Positions

rb.transactions <- merge(rb.enter, rb.exit, by.x = c("exit"), by.y = c("index"))
rb.positions <- rb.transactions[, .(Position = .I,
                                    Direction = ifelse(side.x == "buy", "Long", "Short"),
                                    EnterDate = Date.x,
                                    EnterPrice = spotPrice.x, 
                                    ExitDate = Date.y,
                                    ExitPrice = spotPrice.y)]
rb.positions[, ProfitLoss := ifelse(Direction == "Long", ExitPrice - EnterPrice, EnterPrice - ExitPrice)]
rb.positions[, Return := ifelse(Direction == "Long", (ExitPrice - EnterPrice)/ExitPrice, (EnterPrice - ExitPrice)/EnterPrice)]

sum(rb.positions$ProfitLoss)

rb.return <- merge(rb.test.data, rb.positions, by.x = c("Date"), by.y = c("EnterDate"), all.x = T)
rb.return[is.na(rb.return$Return)] <- 0

rb.return <- (cumprod(1 + rb.return$Return) - 1)[nrow(rb.test.data)] * 100
rb.return <- ( rb.return ^ 1/12) * 12
rb.return

rb.ret <- (cumprod(1 + rb.test.data$return) - 1)[nrow(rb.test.data)] * 100
rb.ret

write.csv(rb.test.data, file = "rb.return.csv")

write.csv(rb.transactions, file = "rb.return.csv")

rb.transactions <- rbind(rb.enter, rb.exit)[ order(Date)][, .(Date, spotPrice, side)]

rb.transactions

write.csv(sc.transactions, file = "sc.trades.csv")

sc.disp.trans <- merge(sc.test.data, sc.transactions, by = c("Date"), all.x = T)
sc.disp.trans$color <- ifelse(sc.disp.trans$side == "buy", "green", ifelse(sc.disp.trans$side == "sell", "red", NA))

sc.disp.trans$spotPrice.y[is.na(sc.disp.trans$spotPrice.y)] <- NA

head(sc.disp.trans, 10)

sc.test.data[, return := (open - close)/close]

ggplot(sc.test.data[Date < "2019-2-1"]) +
  geom_line(aes(x = Date, y = close), lwd = 1, col = "black") +
  geom_point(data = sc.disp.trans[Date < "2019-2-1"], aes(x = Date, y = spotPrice.y, col = side), lwd = 5) +
  labs(title = "WTI Actual vs. Pred", y = "Return") +
  theme(legend.position = "none")

symbol <- "sc"
start_date <- "2019-1-1"

candlestick(symbol, start_date, commodites)

dates <- sc.test.data$Date
sc.holdings <- getHoldingsFromPositions(sc.positions)

write.csv(sc.holdings, file = "sc.positions.csv")

candlestickHoldings("cl", sc.holdings, sc.test.data, 18.79 )
