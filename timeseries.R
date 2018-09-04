####libraries####
library("forecast")

####> grouping by####
head(consHourly)

#in kWh
####by month/year####
ActiveMY <- consHourly %>% group_by(MonthYear) %>%
  summarise(ActivePower = sum(ActivePower, na.rm = T))

####by week/year####
ActiveWY <- consHourly %>% group_by(WeekYear) %>%
  summarise(ActivePower = sum(ActivePower, na.rm = T))

####by quarter/year####
ActiveQY <- consHourly %>% group_by(Quarter) %>%
  summarise(ActivePower = sum(ActivePower, na.rm = T))

####by season/year####
ActiveSY <- consHourly %>% group_by(SeasonYear) %>%
  summarise(ActivePower = sum(ActivePower, na.rm = T))

####> time series####
#month / 12 (months) observations per year
monthTS <- ts(ActiveMY$ActivePower, frequency = 12, start = c(2007))

#week / 52 (weeks) obs per year
weekTS <- ts(ActiveWY$ActivePower, frequency = 52, start = c(2007))

#quarter / 4 obs per year
quarterTS <- ts(ActiveQY$ActivePower, frequency = 4, start = c(2007))

#season / 4 obs per year
seasonTS <- ts(ActiveSY$ActivePower, frequency = 4, start = c(2007))

####plots####
autoplot(monthTS)
autoplot(weekTS)
autoplot(quarterTS)
autoplot(seasonTS)

####> forecasting####
forecast(monthTS)
forecast(weekTS)
forecast(quarterTS)
forecast(seasonTS)

summary(monthTS)
summary(weekTS)
summary(quarterTS)
summary(seasonTS)

####TSLM####
#month
fitMonth <- tslm(monthTS ~ trend + season)
plot(forecast(fitMonth, h = 12))
plot(forecast(fitMonth, h = 60))
#with season and trend, we can see the energy consumption slightly going down with time

#week
fitWeek <- tslm(weekTS ~ trend + season)
plot(forecast(fitWeek, h = 520))

#quarter
fitQuarter <- tslm(quarterTS ~ trend + season)
plot(forecast(fitQuarter, h = 40))

#season
fitSeason <- tslm(seasonTS ~ trend + season)
plot(forecast(fitSeason, h = 40))

####Holt Winters forecast method####
#month
monthHWTS <- HoltWinters(monthTS,
                         alpha = 0.01,
                         beta = 0.1,
                         gamma = T)

monthHW <- forecast(monthHWTS, h = 12, findfrequency = TRUE)
autoplot(monthHW)

####month####
autoplot(forecast(HoltWinters(monthTS), h = 12)) +
  ggtitle("Monthly Global Active Power forecast using Holt Winters") +
  ylab("Global Active Power") + xlab("Year") +
  theme(plot.title = element_text(hjust = 0.5))

####week####
autoplot(forecast(HoltWinters(weekTS), h = 52)) +
  ggtitle("Yearly Global Active Power forecast using Holt Winters") +
  ylab("Global Active Power") + xlab("Year") +
  theme(plot.title = element_text(hjust = 0.5))

####quarter####
autoplot(forecast(HoltWinters(quarterTS), h = 10)) +
  ggtitle("Quarterly Global Active Power forecast using Holt Winters") +
  ylab("Global Active Power") + xlab("Year") +
  theme(plot.title = element_text(hjust = 0.5))

####season####
autoplot(forecast(HoltWinters(seasonTS), h = 10)) +
  ggtitle("Seasonally Global Active Power forecast using Holt Winters") +
  ylab("Global Active Power") + xlab("Year") +
  theme(plot.title = element_text(hjust = 0.5))

####> decomposing####
monthDS <- decompose(monthTS)
weekDS <- decompose(weekTS)
quarterDS <- decompose(quarterTS)
seasonDS <- decompose(seasonTS)

summary(monthDS)
summary(weekDS)
summary(quarterDS)
summary(seasonDS)

####plots####
autoplot(monthDS)
autoplot(weekDS)
autoplot(quarterDS)
autoplot(seasonDS)